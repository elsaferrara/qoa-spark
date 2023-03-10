with Unchecked_Conversion;
package body Qoa
with SPARK_Mode
is
   function uns_to_int is new Unchecked_Conversion (Unsigned_64, Integer_16);

   function Clamp (V : Integer;
                   Min : Integer;
                   Max : Integer)
                   return Integer
   is
   begin
      if V < Min then
         return Min;
      else
         if V > Max then
            return Max;
         else
            return V;
         end if;
      end if;
   end Clamp;


   function Frame_Size (Channels : Unsigned_32; Slices : Unsigned_32)
                        return Unsigned_32
   is
   begin
      return 8 + LMS_LEN * 4 * Channels + 8 * Slices * Channels;
   end Frame_Size;

   procedure Write_u64 (Val : Unsigned_64; Bytes : in out Storage_Array;
                        Index : in out Storage_Offset)
   is
   begin
      Bytes (Index) := Storage_Element ((Val / (2 ** 56)) and 16#ff#);
      Bytes (Index + 1) := Storage_Element ((Val / (2 ** 48)) and 16#ff#);
      Bytes (Index + 2) := Storage_Element ((Val / (2 ** 40)) and 16#ff#);
      Bytes (Index + 3) := Storage_Element ((Val / (2 ** 32)) and 16#ff#);
      Bytes (Index + 4) := Storage_Element ((Val / (2 ** 24)) and 16#ff#);
      Bytes (Index + 5) := Storage_Element ((Val / (2 ** 16)) and 16#ff#);
      Bytes (Index + 6) := Storage_Element ((Val / (2 ** 8)) and 16#ff#);
      Bytes (Index + 7) := Storage_Element ((Val / (2 ** 0)) and 16#ff#);
      Index := Index + 8;
   end Write_u64;


   ----------------
   -- Prediction --
   ----------------

   function Lms_Predict (Lms : Qoa_Lms_t) return Integer is
      Prediction : Integer_64 := 0;
   begin
      for i in 0 .. LMS_LEN - 1 loop
         Prediction := Prediction +
           Integer_64 (Lms.Weights (i) * Lms.History (i));
         null;
      end loop;
      return Integer ((Prediction - (Prediction mod 2**13))/ 2**13); -- Shift_Right
   end Lms_Predict;

   --------------
   --- Update ---
   --------------

   procedure Lms_Update (Lms : in out Qoa_Lms_t;
                         Sample : Integer;
                         Residual : Integer)
   is
      d : Integer_16 := Integer_16 ((Residual - (Residual mod 2**4))/ 2**4);
   begin
      for i in 0 .. LMS_LEN - 1 loop
         if Lms.History (i) < 0 then
            Lms.Weights (i) := Lms.Weights (i) - d;
         else
            Lms.Weights (i) := Lms.Weights (i) +  d;
         end if;
      end loop;
      for i in 0 .. LMS_LEN - 2 loop
         Lms.History (i) := Lms.History (i + 1);
      end loop;
      Lms.History (LMS_LEN - 1) :=  Integer_16 (Sample);
   end Lms_Update;

   --------------
   -- Read_u64 --
   --------------

   procedure Read_u64 (Res : out Unsigned_64; P : in out Storage_Count;
                       Data : Storage_Array)
   is
      R0 : constant Unsigned_64 := Unsigned_64 (Data (P));
      R1 : constant Unsigned_64 := Unsigned_64 (Data (P + 1));
      R2 : constant Unsigned_64 := Unsigned_64 (Data (P + 2));
      R3 : constant Unsigned_64 := Unsigned_64 (Data (P + 3));
      R4 : constant Unsigned_64 := Unsigned_64 (Data (P + 4));
      R5 : constant Unsigned_64 := Unsigned_64 (Data (P + 5));
      R6 : constant Unsigned_64 := Unsigned_64 (Data (P + 6));
      R7 : constant Unsigned_64 := Unsigned_64 (Data (P + 7));
   begin
      Res := Shift_Left (R0, 56)
        or Shift_Left (R1, 48)
        or Shift_Left (R2, 40)
        or Shift_Left (R3, 32)
        or Shift_Left (R4, 24)
        or Shift_Left (R5, 16)
        or Shift_Left (R6, 8)
        or Shift_Left (R7, 0);
      P := P + 8;
   end Read_u64;


   function Div (V : Integer;
                 Scalefactor : Integer)
                 return Integer
   is
      Reciprocal : constant Integer := RECIPROCAL_TAB (Scalefactor);
      Tmp : constant Long_Integer := Long_Integer(V) * Long_Integer(Reciprocal)
        + (1 * (2 ** 15));
      N : constant Integer := Integer ((Tmp - (Tmp mod 2**16))/ 2**16);
   begin
      if V > 0 and then N < 0 then
         return N + 2;
      elsif V < 0 and then N > 0 then
         return N - 2;
      elsif V > 0 and then N = 0 then
         return N + 1;
      elsif V < 0 and then N = 0 then
         return N - 1;
      elsif V = 0 and then N > 0 then
         return N - 1;
      elsif V = 0 and then N < 0 then
         return N + 1;
      else
         return N;
      end if;
   end Div;

   function Encode_Worst_Case (Qoa : Qoa_Desc) return Storage_Count is
      Num_Frames : Unsigned_32 :=
        (Unsigned_32 (Qoa.Samples) + GEN_FRAME_LEN - 1) / GEN_FRAME_LEN;
      Num_Slices : Unsigned_32 :=
        (Unsigned_32 (Qoa.Samples) + SLICE_LEN - 1) / SLICE_LEN;
      Encoded_Size : Unsigned_32 := 8 +
        Num_Frames * 8 +
          Num_Frames * LMS_LEN * 4 * Unsigned_32 (Qoa.Channels) +
        Num_Slices * 8 * Unsigned_32 (Qoa.Channels);
   begin
      return Storage_Count (Encoded_Size);
   end Encode_Worst_Case;


   procedure Encode (Sample_Data :     Output_Array;
                     Qoa         :     in out Qoa_Desc;
                     Output      : out Storage_Array;
                     Output_len  : out Storage_Count)
   is

      function Encode_Header (Qoa : Qoa_Desc; Bytes : Storage_Array)
                              return Storage_Count
      is
         P : Storage_Count := Bytes'First;
      begin
         Write_u64 ((Unsigned_64 (MAGIC)) * (2 ** 32) or
                      (Unsigned_64 (Qoa.samples)), Output, P);
         return P;
      end Encode_Header;

      procedure Encode_Frame (Sample_Data : Output_Array;
                              Qoa : in out Qoa_Desc;
                              Bytes : in out Storage_Array;
                              Index : in out Storage_Count;
                              F_Len : Integer)
      is
         Channels : constant Unsigned_32 := Unsigned_32 (Qoa.Channels);
         Slices : constant Integer := (F_Len + SLICE_LEN - 1)
           / SLICE_LEN;
         F_Size : constant Unsigned_32 :=
           Frame_Size (Channels, Unsigned_32 (Slices));
         Weights : Unsigned_64;
         History : Unsigned_64;
         Sample_Index : Integer := 0;

         Slice_Size : Integer;
         Slice_Start : Integer;
         Slice_End : Integer;

         Best_Error : Unsigned_64;
         Best_Slice : Unsigned_64;
         Best_Lms : qoa_lms_t;

         Lms : qoa_lms_t;
         Slice : Unsigned_64;
         Current_Error : Unsigned_64;
         SI : Integer;

         Sample : Integer;
         Predicted : Integer;
         Residual : Integer;
         Scaled : Integer;
         Clamped : Integer;
         Quantized : Integer;
         Dequantized : Integer;
         Reconstructed : Integer;
         Error : Long_Long_Integer;

      begin

         Write_u64 ((
                    Unsigned_64 (Qoa.Channels) * (2 ** 56) +
                      Unsigned_64 (Qoa.Samplerate) * (2 ** 32) +
                      Unsigned_64 (F_Len) * (2 ** 16) +
                      Unsigned_64 (F_Size)
                   ), Bytes, Index);

         --  Write the current LMS state
         for c in Integer range 0 .. (Integer (Channels) - 1) loop
            Weights := 0;
            History := 0;
            for i in 0 .. LMS_LEN - 1 loop
               History := Shift_Left (History, 16) or
                 (Unsigned_64'Mod (Qoa.Lms (c) .History (i)) and 16#ffff#);
               Weights := Shift_Left (Weights, 16) or
                 (Unsigned_64'Mod (Qoa.Lms (c) .Weights (i)) and 16#ffff#);
            end loop;
            Write_u64 (History, Bytes, Index);
            Write_u64 (Weights, Bytes, Index);
         end loop;

         --  Encode all samples
         while Sample_Index < F_Len loop
            for c in Integer range 0 .. Integer (Channels) - 1 loop
               Slice_Size := Clamp (SLICE_LEN, 0,
                                    F_Len - Sample_Index);

               Slice_Start := Sample_Index * Integer (Channels) + c;
               Slice_End := (Sample_Index + Slice_Size)
                 * Integer (Channels) + c;

               --  Search the best scalefactor. Go through all 16 scalefactors,
               --  encode all samples for the current slice and
               --  measure the total squared error.
               Best_Error := -1;

               for Scalefactor in 0 .. 15 loop

                  --  Reset the LMS state to the last known good one
                  --  before trying each scalefactor

                  Lms := Qoa.Lms (c);
                  Slice := Unsigned_64 (Scalefactor);
                  Current_Error := 0;

                  SI := Slice_Start;
                  while SI < Slice_End loop
                     Sample :=
                       Integer (Sample_Data (Sample_Data'First + Storage_Count(SI)));

                     Predicted := Lms_Predict (Lms);
                     Residual := Sample - Predicted;

                     Scaled := Div (Residual, Scalefactor);
                     Clamped := Clamp (Scaled, -8, 8);
                     Quantized := QUANT_TAB (Clamped);
                     Dequantized := DEQUANT_TAB (Scalefactor, Quantized);
                     Reconstructed := Clamp (Predicted + Dequantized,
                                             -32768, 32767);

                     Error := Long_Long_Integer (Sample - Reconstructed);
                     Current_Error := Current_Error +
                       Unsigned_64 (Error * Error);

                     if Current_Error > Best_Error then
                        exit;
                     end if;

                     Lms_Update (Lms, Reconstructed, Dequantized);
                     Slice := (Slice * (2 ** 3)) or Unsigned_64 (Quantized);

                     SI := SI + Integer (Channels);
                  end loop;

                  if Current_Error < Best_Error then
                     Best_Error := Current_Error;
                     Best_Slice := Slice;
                     Best_Lms := Lms;
                  end if;
               end loop;

               Qoa.Lms (c) := Best_Lms;
               Best_Slice := Shift_Left (Best_Slice,
                                         ((SLICE_LEN - Slice_Size) * 3));
               Write_u64 (Best_Slice, Bytes, Index);
            end loop;
            Sample_Index := Sample_Index + SLICE_LEN;
         end loop;
      end Encode_Frame;


      Index : Storage_Count;
      F_Len : Integer := GEN_FRAME_LEN;
      Sample_Index : Integer := 0;
      Input_First : Storage_Count;
   begin

      --  initialize LMS weights to {0, 0, -1, 2}
      for c in Integer range 0 .. Integer (Qoa.Channels) - 1 loop
         Qoa.Lms (c) .Weights (0) := 0;
         Qoa.Lms (c) .Weights (1) := 0;
         Qoa.Lms (c) .Weights (2) := -1 * (2 ** 13);
         Qoa.Lms (c) .Weights (3) :=  1 * (2 ** 14);

         --  initialize LMS weights to {0, 0, 0, 0}
         for i in 0 .. LMS_LEN - 1 loop
            Qoa.Lms (c) .History (i) := 0;
         end loop;
      end loop;

      --  Encode the header and go through all frames
      Index := Encode_Header (Qoa, Output);

      while Sample_Index < Integer (Qoa.Samples) loop
         F_Len := Clamp (GEN_FRAME_LEN, 0, Integer (Qoa.Samples) - Sample_Index);
         Input_First := Sample_Data'First +
           Storage_Count (Sample_Index) * Qoa.channels;
         Encode_Frame (Sample_Data (Input_First .. Sample_Data'Last),
                       Qoa, Output, Index, F_Len);
         Sample_Index := Sample_Index + F_Len;

      end loop;
      Output_len := Index - Output'First;
   end Encode;




   procedure Decode_Header (Data : Storage_Array;
                            Qoa : out Qoa_Desc)
   is

      In_Index : Storage_Count:= Data'First ;
      File_Header : Unsigned_64;
      Frame_Header : Unsigned_64;

   begin

      if Data'Length < MIN_FILESIZE then
         return;
      end if;

      Read_u64 (File_Header, In_Index, Data);
      if Shift_Right (File_Header, 32) /= MAGIC then
         return;
      end if;

      Qoa.Samples := Storage_Count (File_Header and 16#ffffffff#);

      Read_u64 (Frame_Header, In_Index, Data);
      Qoa.Channels :=
        Storage_Count (Shift_Right (Frame_Header, 56) and 16#0000ff#);
      Qoa.Samplerate :=
        Storage_Count (Shift_Right (Frame_Header, 32) and 16#ffffff#);

      if Qoa.Channels = 0 or else Qoa.Samples = 0
        or else Qoa.Samplerate = 0
      then
         return;
      end if;
   end Decode_Header;

   procedure Decode (Data        :     Storage_Array;
                     Qoa        : out Qoa_Desc;
                     Output      : out Output_Array;
                     Output_Size : out Storage_Count)
   is
      P : Storage_Count;
      Index : Storage_Count := Output'First;



      procedure Decode_Frame (Data : Storage_Array;
                              In_Index : in out Storage_Count;
                              Qoa : in out Qoa_Desc;
                              Output : in out Output_Array;
                              Index : Storage_Count;
                              Frame_Len : out Unsigned_32;
                              End_Of_Decode : out Boolean)
      is

         Frame_Header : Unsigned_64;
         Channels : Integer;
         Samplerate : Integer;
         F_Samples : Integer;
         Frame_Size : Integer;

         Data_Size : Integer;
         Num_Slices : Integer;
         Max_Total_Samples : Integer;

         History : Unsigned_64;
         Weights : Unsigned_64;

         Sample_I : Integer := 0;
         Slice : Unsigned_64;
         Scalefactor : Integer;
         Slice_Start : Integer;
         Slice_End  : Integer;
         SI : Integer;

         Predicted : Integer;
         Quantized : Integer;
         Dequantized : Integer;
         Reconstructed : Integer;


      begin

         if Data'Length < 16 + LMS_LEN * 4 * Qoa.Channels then
            End_Of_Decode := True;
            return;
         end if;

         --  Read and verify the frame header
         Read_u64 (Frame_Header, In_Index, data);
         Channels := Integer (Shift_Right (Frame_Header, 56) and 16#0000ff#);
         Samplerate := Integer (Shift_Right (Frame_Header, 32) and 16#ffffff#);
         F_Samples    := Integer (Shift_Right (Frame_Header, 16) and 16#00ffff#);
         Frame_Size := Integer ((Frame_Header) and 16#00ffff#);
         Data_Size := Frame_Size - 8 - LMS_LEN * 4 * Channels;
         Num_Slices := Data_Size / 8;
         Max_Total_Samples := Num_Slices * SLICE_LEN;

         if Channels /= Integer (Qoa.Channels) or else
           Samplerate /= Integer (Qoa.Samplerate) or else
           Frame_Size > Data'Length or else
           F_Samples * Channels > Max_Total_Samples

         then
            End_Of_Decode := True;
            return;
         end if;

         --  Read the LMS state: 4 x 2 bytes history,
         --  4 x 2 bytes weights per channel
         for c in 0 .. Channels - 1 loop
            Read_u64 (History, In_Index, Data);
            Read_u64 (Weights, In_Index, Data);
            for i in 0 .. LMS_LEN - 1 loop
               Qoa.Lms (c) .History (i) :=
                 uns_to_int (Shift_Right (History, 48));
               History := Shift_Left (History, 16);
               Qoa.Lms (c) .Weights (i) :=
                 uns_to_int (Shift_Right (Weights, 48));
               Weights := Shift_Left (Weights, 16);
            end loop;
         end loop;

         --  Decode all slices for all channels in this frame

         while Sample_I < F_Samples loop

            for c in 0 .. Channels - 1 loop
               Read_u64 (Slice, In_Index, Data);
               Scalefactor := Integer (Shift_Right (Slice, 60) and 16#f#);
               Slice_Start := Sample_I * Channels + c;
               Slice_End := Clamp (Sample_I + SLICE_LEN, 0, F_Samples)
                 * Channels + c;

               SI := Slice_Start;
               while SI < Slice_End loop

                  Predicted := Lms_Predict (Qoa.Lms (c));
                  Quantized := Integer (Shift_Right (Slice, 57) and 16#7#);
                  Dequantized := DEQUANT_TAB (Scalefactor, Quantized);
                  Reconstructed := Clamp (Predicted + Dequantized,
                                          -32768, 32767);
                  Output (Index + Storage_Count (SI)) := Integer_16 (Reconstructed);
                  Slice := Shift_Left (Slice, 3);
                  Lms_Update (Qoa.Lms (c), Reconstructed, Dequantized);
                  SI := SI + Channels;
               end loop;
            end loop;
            Sample_I := Sample_I + SLICE_LEN;
         end loop;

         Frame_Len := Unsigned_32 (F_Samples);
         End_Of_Decode := False;

      end Decode_Frame;

      Sample_Index : Unsigned_32 := 0;
      Frame_Len : Unsigned_32;
      End_Of_Decode : Boolean;

   begin

      P := Data'First;
      Decode_Header (Data, Qoa);
      P := Data'First + HEADER_SIZE;

      loop

         Index := Storage_Count (Sample_Index) * Qoa.channels;
         Decode_Frame (Data, P, Qoa, Output, Index,Frame_Len, End_Of_Decode);
         Sample_Index := Sample_Index + Frame_Len;
         if End_Of_Decode or Sample_Index >= Unsigned_32 (Qoa.Samples) then
            exit;
         end if;
      end loop;

      Qoa.Samples := Storage_Count (Sample_Index);
      Index := Storage_Count (Sample_Index) * Qoa.Channels;
      Output_Size := Index - Output'First;
   end Decode;

end Qoa;
