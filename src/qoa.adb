with Unchecked_Conversion;
package body Qoa
with SPARK_Mode
is

   function Uns_To_Int is new Unchecked_Conversion (Unsigned_16, Integer_16);

   function Frame_Size (Channels : Unsigned_16; Slices : Unsigned_16)
                        return Unsigned_16

   is
   begin
      return 8 + LMS_LEN * 4 * Channels + 8 * Slices * Channels;
   end Frame_Size;

   procedure Write_u64 (Val : Unsigned_64; Bytes : in out Storage_Array;
                        Index : in out Storage_Count)
     --  with SPARK_Mode => Off
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

   --------------
   -- Read_u64 --
   --------------

   procedure Read_u64 (Res : out Unsigned_64; P : in out Storage_Count;
                       Data : Storage_Array)
     --  with SPARK_Mode => Off
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

   ----------------
   -- Prediction --
   ----------------

   function Lms_Predict (Lms : Qoa_Lms_t) return Integer
   is
      Prediction : Integer_64 := 0;
   begin
      for i in 0 .. LMS_LEN - 1 loop
         Prediction := Prediction +
           Integer_64 (Lms.Weights (i)) * Integer_64 (Lms.History (i));
         null;
      end loop;
      return Integer ((Prediction - (Prediction mod 2**13))/ 2**13); -- Shift_Right
   end Lms_Predict;

   --------------
   --- Update ---
   --------------

   procedure Lms_Update (Lms : in out Qoa_Lms_t;
                         Sample : Integer_16;
                         Residual : Integer)
     with SPARK_Mode => Off
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
      Lms.History (LMS_LEN - 1) :=  Sample;
   end Lms_Update;

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

   function Clamp (V : Integer;
                   Min : Unsigned_16;
                   Max : Unsigned_16)
                   return Unsigned_16
   is
   begin
      if V < Integer (Min) then
         return Min;
      else
         if V > Integer (Max) then
            return Max;
         else
            return Unsigned_16 (V);
         end if;
      end if;
   end Clamp;

   function Clamp (V : Unsigned_16;
                   Max : Unsigned_32)
                      return Unsigned_16
   is
   begin
      if Unsigned_32 (V) > Max then
         return Unsigned_16 (Max);
      else
         return V;
      end if;
   end Clamp;

      function Clamp (V : Positive_16;
                   Max : Positive_16)
                      return Positive_16
   is
   begin
      if V > Max then
         return Max;
      else
         return V;
      end if;
   end Clamp;

   function Clamp_16 (V : Integer)
                         return Integer_16
   is
   begin
      if V < Integer (Integer_16'First) then
         return Integer_16'First;
      else
         if V > Integer (Integer_16'Last) then
            return Integer_16'Last;
         else
            return Integer_16 (V);
         end if;
      end if;
   end Clamp_16;

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

   --  function Encode_Worst_Case (Qoa : Qoa_Desc) return Storage_Count
   --  is
   --     Num_Frames : Unsigned_32 :=
   --       (Unsigned_32 (Qoa.Samples) + GEN_FRAME_LEN - 1) / GEN_FRAME_LEN;
   --     Num_Slices : Unsigned_32 :=
   --       (Unsigned_32 (Qoa.Samples) + SLICE_LEN - 1) / SLICE_LEN;
   --     Encoded_Size : Unsigned_32 := 8 +
   --       Num_Frames * 8 +
   --         Num_Frames * LMS_LEN * 4 * Unsigned_32 (Qoa.Channels) +
   --       Num_Slices * 8 * Unsigned_32 (Qoa.Channels);
   --  begin
   --     return Storage_Count (Encoded_Size);
   --  end Encode_Worst_Case;

   function Encode_Size (Channels : Channels_Type; Samples : Samples_Type ) return Storage_Count
   is
      (8 +
        (Samples + GEN_FRAME_LEN - 1) / GEN_FRAME_LEN * 8 +
          (Samples + GEN_FRAME_LEN - 1) / GEN_FRAME_LEN * LMS_LEN * 4 * Channels +
           (Samples + SLICE_LEN - 1) / SLICE_LEN * 8 * Channels);

      function Frame_Size (Channels : Channels_Type; F_Len : Unsigned_16 ) return Storage_Count
   is
      (8 + 16 * Channels +
            (Storage_Count (F_Len) + SLICE_LEN - 1) / SLICE_LEN * 8 * Channels);


   procedure Encode (Sample_Data :     Output_Array;
                     Qoa         :     in out Qoa_Desc;
                     Output      : out Storage_Array;
                     Output_len  : out Storage_Count)
   is
      procedure Encode_Header (Qoa : Qoa_Desc; Output : in out Storage_Array;
                               P : in out Storage_Count)
        with
          Relaxed_Initialization => Output,
          Pre => Output'First >= 0
          and then Output'Last < Storage_Count'Last
          and then Qoa.Samples <= 2 ** 32
          and then (Qoa.Samples + SLICE_LEN - 1) / SLICE_LEN < Storage_Count'Last / 256
          and then Output'Length = Encode_Size (Qoa.Channels,Qoa.Samples)
          and then P = Output'First,

          Post => P = P'Old + 8
            ;

      procedure Encode_Header (Qoa : Qoa_Desc; Output : in out Storage_Array;
                               P : in out Storage_Count)
      is
      begin
         Write_u64 ((Unsigned_64 (MAGIC)) * (2 ** 32) or
                      (Unsigned_64 (Qoa.samples)), Output, P);
      end Encode_Header;

      procedure Encode_Frame (Sample_Data : Output_Array;
Input_First : Storage_Count;
                              Qoa : in out Qoa_Desc;
                              Bytes : in out Storage_Array;
                              Index : in out Storage_Count;
                              F_Len : Unsigned_16)
        with
          Relaxed_Initialization => Bytes,
          Pre =>
            Index >= Bytes'First
            and then F_Len in 1 .. GEN_FRAME_LEN
              --  and then Bytes'Last >= Storage_Count'First + Frame_Size (Qoa.Channels, F_Len)
              and then Qoa.Samples <= 2 ** 32
              and then (Qoa.Samples + SLICE_LEN - 1) / SLICE_LEN < Storage_Count'Last / 256
          and then Bytes'Length = Integer_128(Encode_Size (Qoa.Channels, Qoa.Samples))
        and then Storage_Count(F_Len) <= Qoa.Samples
              and then Index  <= Bytes'Last - (Frame_Size (Qoa.Channels, F_Len)) + 1
              and then  (F_Len + SLICE_LEN - 1) / SLICE_LEN >= 0
          and then Bytes'Last < Storage_Count'Last
--              ,
--  Post => Index >= Bytes'First
        --  Bytes'Length = Encode_Size (Qoa.Channels,Qoa.Samples)




         ;
      procedure Encode_Frame (Sample_Data : Output_Array;
                              Input_First : Storage_Count;
                              Qoa : in out Qoa_Desc;
                              Bytes : in out Storage_Array;
                              Index : in out Storage_Count;
                              F_Len : Unsigned_16)
      is
         Channels : constant Unsigned_16 := Unsigned_16 (Qoa.Channels);
         Slices : constant Unsigned_16 := Unsigned_16 ((F_Len + SLICE_LEN - 1)
                                                       / SLICE_LEN);
         F_Size : constant Unsigned_16 :=
           Frame_Size (Channels, Slices);
         Weights : Unsigned_64;
         History : Unsigned_64;
         Sample_Index : Unsigned_16 := 0;

         Slice_Size : Unsigned_16;
         Slice_Start : Unsigned_16;
         Slice_End : Unsigned_16;


         Best_Error : Unsigned_64;
         Best_Slice : Unsigned_64 := 0;
         Best_Lms : qoa_lms_t;

         Lms : qoa_lms_t;
         Slice : Unsigned_64;
         Current_Error : Unsigned_64;
         SI : Unsigned_16;

         Sample : Integer_16;
         Predicted : Integer;
         Residual : Integer;
         Scaled : Integer;
         Clamped : Integer;
         Quantized : Integer;
         Dequantized : Integer;
         Reconstructed : Integer_16;
         Error : Integer;

         Index_Ref : constant Storage_Count := Index with Ghost;


      begin

         Write_u64 ((
                    Unsigned_64 (Qoa.Channels) * (2 ** 56) +
                      Unsigned_64 (Qoa.Samplerate) * (2 ** 32) +
                      Unsigned_64 (F_Len) * (2 ** 16) +
                      Unsigned_64 (F_Size)
                   ), Bytes, Index);

         --  Write the current LMS state
         for c in Integer range 0 .. (Integer (Channels) - 1) loop
--  pragma Loop_Invariant (Bytes'Length = Encode_Size (Qoa.Channels,Qoa.Samples));
            --  pragma Loop_Invariant (Index >= Bytes'First);
            --  pragma Loop_Invariant (Bytes'Length = Encode_Size (Qoa.Channels, Qoa.Samples));
            pragma Loop_Invariant (Index = Index_Ref + 8 + 16 * Storage_Count(c));
            pragma Loop_Invariant (Index  <= Bytes'Last - (16 * (Qoa.Channels - Storage_Count(c)) +
            (Storage_Count (F_Len) + SLICE_LEN - 1) / SLICE_LEN * 8 * Qoa.Channels) + 1 );
--  pragma Loop_Invariant (Index  );
            --  pragma Loop_invariant (Index <= Storage_Count'Last - 16);
            Weights := 0;
            History := 0;
            for i in 0 .. LMS_LEN - 1 loop
               pragma Assert (Channels <= MAX_CHANNELS);
               History := Shift_Left (History, 16) or
                 (Unsigned_64'Mod (Qoa.Lms (c) .History (i)) and 16#ffff#);
               Weights := Shift_Left (Weights, 16) or
                 (Unsigned_64'Mod (Qoa.Lms (c) .Weights (i)) and 16#ffff#);
            end loop;

            Write_u64 (History, Bytes, Index);
            Write_u64 (Weights, Bytes, Index);

         end loop;
         pragma Assert (Index = Index_Ref + 8 + 16 * Storage_Count(Channels));
            pragma Assert (Index  <= Bytes'Last - (
            (Storage_Count (F_Len) + SLICE_LEN - 1) / SLICE_LEN * 8 * Qoa.Channels) + 1 );
         --  Encode all samples
         while Sample_Index < F_Len loop
            pragma Loop_Invariant (Sample_Index < F_Len);
            pragma Loop_Invariant (Sample_Index Mod SLICE_LEN = 0);
            pragma Loop_Invariant (Index = Index_Ref + 8 + 16 * Storage_Count(Channels)
                                   + Storage_Count(Sample_Index)
                                                       / SLICE_LEN * Storage_Count(Channels) * 8);
            for c in 0 .. Channels - 1 loop
               pragma Loop_Invariant (Index = Index_Ref + 8 + 16 * Storage_Count(Channels) + Storage_Count(Sample_Index)
                                                       / SLICE_LEN * Storage_Count(Channels) * 8 + 8 * Storage_Count (c));

               Slice_Size := Clamp (SLICE_LEN, 0, F_Len - Sample_Index);
               Slice_Start := Sample_Index * Channels + c;
               Slice_End := (Sample_Index + Slice_Size) * Channels + c;

               --  Search the best scalefactor. Go through all 16 scalefactors,
               --  encode all samples for the current slice and
               --  measure the total squared error.
               Best_Error := -1;

               for Scalefactor in 0 .. 15 loop
                  pragma Loop_Invariant (True);
                  --  Reset the LMS state to the last known good one
                  --  before trying each scalefactor

                  Lms := Qoa.Lms (Integer(c));
                  Slice := Unsigned_64 (Scalefactor);
                  Current_Error := 0;


                  SI := Slice_Start;

                  while SI < Slice_End loop
                    Sample := Sample_Data (Input_First + Storage_Count(SI));

                     Predicted := Lms_Predict (Lms);
                     Residual := Integer (Sample) - Predicted;

                     Scaled := Div (Residual, Scalefactor);
                     Clamped := Clamp (Scaled, -8, 8);
                     Quantized := QUANT_TAB (Clamped);
                     Dequantized := DEQUANT_TAB (Scalefactor, Quantized);
                     Reconstructed := Clamp_16 (Predicted + Dequantized);

                     Error := Integer (Sample) - Integer (Reconstructed);

                     Current_Error := Current_Error +
                       Unsigned_64 (Abs (Error)) * Unsigned_64 (Abs (Error));

                     if Current_Error > Best_Error then
                        exit;
                     end if;

                     Lms_Update (Lms, Reconstructed, Dequantized);
                     Slice := (Slice * (2 ** 3)) or Unsigned_64 (Quantized);
                     SI := SI + Channels;
                  end loop;

                  if Current_Error < Best_Error then
                     Best_Error := Current_Error;
                     Best_Slice := Slice;
                     Best_Lms := Lms;
                  end if;

               end loop;

               Qoa.Lms (Integer(c)) := Best_Lms;
               Best_Slice := Shift_Left (Best_Slice,
                                         Integer ((SLICE_LEN - Slice_Size) * 3));
               Write_u64 (Best_Slice, Bytes, Index);

            end loop;
            pragma Assert (Index = Index_Ref + 8 + 16 * Storage_Count(Channels) + Storage_Count(Sample_Index)
                                                       / SLICE_LEN * Storage_Count(Channels) * 8 + 8 * Storage_Count (Channels));
            pragma Assert (Sample_Index < Unsigned_16'Last - SLICE_LEN);

            Sample_Index := Sample_Index + SLICE_LEN;
--  pragma Assert (Index = Index_Ref + 8 + 16 * Storage_Count(Channels) + (Storage_Count(Sample_Index) - SLICE_LEN)
--                                                         / SLICE_LEN  * Storage_Count(Channels) * 8 + 8 * Storage_Count (Channels));
--
--  pragma Assert (Index = Index_Ref + 8 + 16 * Storage_Count(Channels) + ((Storage_Count(Sample_Index) - SLICE_LEN)
--                                                         / SLICE_LEN + 1 ) * Storage_Count(Channels) * 8 );
--
            --  pragma Assert (Index = Index_Ref + 8 + 16 * Storage_Count(Channels) + Storage_Count(Sample_Index)
            --                 / SLICE_LEN * Storage_Count(Channels) * 8 );
         end loop;


      end Encode_Frame;


      Index : Storage_Count := Output'First;
      F_Len : Unsigned_16; -- := GEN_FRAME_LEN;
      Sample_Index : Unsigned_32 := 0;
      Input_First : Storage_Count;
   begin

      --  initialize LMS weights to {0, 0, -1, 2}
      for c in Integer range 0 .. Integer (Qoa.Channels) - 1 loop
         pragma Loop_Invariant (Output'Length = Encode_Size (Qoa.Channels,Qoa.Samples));

         Qoa.Lms (c) .Weights (0) := 0;
         Qoa.Lms (c) .Weights (1) := 0;
         Qoa.Lms (c) .Weights (2) := -1 * (2 ** 13);
         Qoa.Lms (c) .Weights (3) :=  1 * (2 ** 14);

         --  initialize LMS weights to {0, 0, 0, 0}
         for i in 0 .. LMS_LEN - 1 loop
            pragma Loop_Invariant (Output'Length = Encode_Size (Qoa.Channels,Qoa.Samples));
            Qoa.Lms (c) .History (i) := 0;
         end loop;
      end loop;
      --  Encode the header and go through all frames
      Encode_Header (Qoa, Output, Index);

      while Sample_Index < Unsigned_32 (Qoa.Samples) loop
--  pragma Loop_Invariant (Index >= Output'First);
--           pragma Loop_Invariant (Output'Length = Encode_Size (Qoa.Channels,Qoa.Samples));

         F_Len := Clamp (GEN_FRAME_LEN, Unsigned_32 (Qoa.Samples) - Sample_Index);
         pragma Assert (Storage_Count (Sample_Index) * Qoa.channels >= 0);
         Input_First := Sample_Data'First +
           Storage_Count (Sample_Index) * Qoa.channels;

         --  Encode_Frame (Sample_Data (Input_First .. Sample_Data'Last),
         --                Qoa, Output, Index, F_Len);
         Encode_Frame (Sample_Data, Input_First,
              Qoa, Output, Index, F_Len);

         Sample_Index := Sample_Index + Unsigned_32 (F_Len);
      end loop;

      if Index < Output'First
      then
         Output_Len := 0;
         return;
      end if;
      Output_len := Index - Output'First;
   end Encode;




   procedure Decode_Header (Data : Storage_Array;
                            Qoa : out Qoa_Desc;
                            End_Of_Header : out Boolean)
   is

      In_Index : Storage_Count := Data'First ;
      File_Header : Unsigned_64;
      Frame_Header : Unsigned_64;
      Temp_32 : Unsigned_32;
      Temp_24 : Unsigned_24;
      Temp_8 : Unsigned_8;

   begin

      if Data'Length < MIN_FILESIZE then
         End_Of_Header := True;
         return;
      end if;

      Read_u64 (File_Header, In_Index, Data);

      if Shift_Right (File_Header, 32) /= MAGIC then
         End_Of_Header := True;
         return;
      end if;
      Temp_32 := Unsigned_32 (File_Header and 16#ffffffff#);
      Qoa.Samples := Storage_Count (Temp_32);
      Read_u64 (Frame_Header, In_Index, Data);
      pragma Assert (In_Index = Data'First + HEADER_SIZE + 8);
      Temp_8 := Unsigned_8 (Shift_Right (Frame_Header, 56) and 16#0000ff#);
      if Temp_8 > MAX_CHANNELS or Temp_8 = 0 then
         End_Of_Header := True;
         return;
      end if;
      Qoa.Channels :=  Storage_Count (Temp_8);
      Temp_24 := Unsigned_24 (Shift_Right (Frame_Header, 32) and 16#ffffff#);
      Qoa.Samplerate := Storage_Count (Temp_24);
      Qoa.Lms := (others => ( History => <>,
                              Weights => <>));

      if Qoa.Samples = 0
        or else Qoa.Samplerate = 0
      then
         End_Of_Header := True;
         return;
      end if;
      End_Of_Header := False;
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
                              Frame_Len : out Unsigned_16;
                              End_Of_Decode : out Boolean)
        with
          Relaxed_Initialization => Output,
          Pre => Data'Last < Storage_Count'Last
          and then Data'First >= 0
          and then Data'Length >= 8 + 16 * Qoa.Channels
          and then Data'Length = Encode_Size (Qoa.Channels,Qoa.Samples)
          and then In_Index <= Data'Last - 7 - 16 * Qoa.Channels
          and then In_Index >= Data'First
          and then Index in Output'Range
          and then Output'Last < Storage_Count'Last
            and then Output'First = 0
--  ,
          --  Post => In_Index >= Data'First
          --  and then Data'Length = Encode_Size (Qoa.Channels,Qoa.Samples)
;

      --  procedure Decode_Frame (Data : Storage_Array;
      --                          In_Index : in out Storage_Count;
      --                          Qoa : in out Qoa_Desc;
      --                          Output : in out Output_Array;
      --                          Index : Storage_Count;
      --                          Frame_Len : out Unsigned_16;
      --                          End_Of_Decode : out Boolean)
      --  is
      --
      --     In_Index_Ref : constant Storage_Count := In_Index with Ghost;
      --
      --     Frame_Header : Unsigned_64;
      --     Channels : Unsigned_16;
      --     Samplerate : Unsigned_24;
      --     F_Samples : Unsigned_16;
      --     Frame_Size : Unsigned_16;
      --
      --     Data_Size : Integer;
      --     Num_Slices : Integer;
      --     Max_Total_Samples : Integer;
      --
      --     History : Unsigned_64;
      --     Weights : Unsigned_64;
      --
      --     Sample_I : Unsigned_16 := 0;
      --     Slice : Unsigned_64;
      --     Scalefactor : Integer;
      --     Slice_Start : Unsigned_16;
      --     Slice_End  : Unsigned_16;
      --     SI : Unsigned_16;
      --
      --     Predicted : Integer;
      --     Quantized : Integer;
      --     Dequantized : Integer;
      --     Reconstructed : Integer_16;
      --
      --     Temp_16 : Unsigned_16;
      --     Clamped : Unsigned_16;
      --     --  SI_Offset : Unsigned_16 with Ghost;
      --     --  Sample_I_Offset : Unsigned_16 with Ghost;
      --
      --
      --  begin
      --
      --     Frame_Len := 0;
      --
      --     if Data'Length < 16 + LMS_LEN * 4 * Qoa.Channels then
      --        End_Of_Decode := True;
      --        return;
      --     end if;
      --
      --     --  Read and verify the frame header
      --     Read_u64 (Frame_Header, In_Index, Data);
      --     Channels := Unsigned_16 (Shift_Right (Frame_Header, 56) and 16#0000ff#);
      --     Samplerate := Unsigned_24 (Shift_Right (Frame_Header, 32) and 16#ffffff#);
      --     F_Samples := Unsigned_16 (Shift_Right (Frame_Header, 16) and 16#00ffff#);
      --     Frame_Size := Unsigned_16 (Frame_Header and 16#00ffff#);
      --     if Frame_Size < 8 + LMS_LEN * 4 * Channels then
      --        End_Of_Decode := True;
      --        return;
      --     end if;
      --     Data_Size := Integer (Frame_Size) - 8 - LMS_LEN * 4 * Integer (Channels);
      --     Num_Slices := Data_Size / 8;
      --     Max_Total_Samples := Num_Slices * SLICE_LEN;
      --
      --     if Channels /= Unsigned_16 (Qoa.Channels) or else
      --       Storage_Count (Samplerate) /= Qoa.Samplerate or else
      --       F_Samples = 0 or else
      --     --  Integer_64 (Frame_Size) > Data'Length or else
      --     In_Index > Data'Last - Storage_Count(Data_Size) or else
      --       Storage_Count (F_Samples) * Storage_Count (Channels) > Output'Last - Index + 1 or else
      --       Integer (F_Samples) * Integer (Qoa.Channels) > Max_Total_Samples
      --
      --     then
      --        End_Of_Decode := True;
      --        return;
      --     end if;
      --     --  pragma Assert (Index + Storage_Count(F_Samples) * Storage_Count(Channels) - 1 >= 0);
      --     --  Read the LMS state: 4 x 2 bytes history,
      --     --  4 x 2 bytes weights per channel
      --     for c in Integer range 0 .. Integer(Channels) - 1 loop
      --        pragma Loop_Invariant (In_Index = In_Index_Ref + 8 + 16 * Storage_Count (c));
      --
      --        Read_u64 (History, In_Index, Data);
      --        Read_u64 (Weights, In_Index, Data);
      --        for i in 0 .. LMS_LEN - 1 loop
      --           Temp_16 := Unsigned_16 (Shift_Right (History, 48));
      --           Qoa.Lms (c) .History (i) :=
      --             Uns_To_Int (Temp_16);
      --           History := Shift_Left (History, 16);
      --           Temp_16 := Unsigned_16 (Shift_Right (Weights, 48));
      --           Qoa.Lms (c) .Weights (i) :=
      --             Uns_To_Int (Temp_16);
      --           Weights := Shift_Left (Weights, 16);
      --           -- Put_Line ("Ada : predict : " & Predicted'Img & " / dequant : " & Dequantized'Img);
      --
      --        end loop;
      --     end loop;
      --     Put_Line ("Channels : " & Channels'Img);
      --     Put_Line ("CF_Sampels : " & F_Samples'Img);
      --     Put_Line ("output last : " & Output'Last'Img);
      --     Put_Line ("Index : " & Index'Img);
      --     --  Decode all slices for all channels in this frame
      --     pragma Assert (Sample_I = 0);
      --     --  Sample_I_Offset := 0;
      --     while Sample_I < F_Samples loop
      --        pragma Loop_Invariant (Index + Storage_Count(F_Samples) * Storage_Count (Channels) - 1 in Output'Range);
      --        pragma Loop_Invariant (In_Index >= Data'First);
      --        --  pragma Loop_Invariant (Sample_I = Sample_I_Offset * SLICE_LEN);
      --        pragma Loop_Invariant (Sample_I < Unsigned_16'Last - SLICE_LEN);
      --        pragma Loop_Invariant (Sample_I Mod SLICE_LEN = 0);
      --        pragma Loop_Invariant (In_Index = In_Index_Ref + 8 +
      --                                 16 * Storage_Count (Channels) +
      --                                 Storage_Count (Channels) * 8 * Storage_Count(Sample_I) / SLICE_LEN);
      --        --  pragma Loop_Invariant (Storage_Count(Slice_End) <= Storage_Count'Last - Index);
      --        --  pragma Loop_Invariant (Storage_Count(Slice_End) <= Output'Last - Index);
      --        pragma Assert (Sample_I + SLICE_LEN > 0);
      --        Clamped := Clamp (Sample_I + SLICE_LEN, F_Samples);
      --        pragma Assert (Clamped > 0);
      --        for c in 0 .. Channels - 1 loop
      --           pragma Loop_Invariant (Index + Storage_Count(F_Samples) * Storage_Count (Channels) - 1 in Output'Range);
      --
      --           pragma Loop_Invariant (In_Index >= Data'First);
      --           pragma Loop_Invariant (In_Index = In_Index_Ref + 8 +
      --                                    16 * Storage_Count (Channels) +
      --                                    Storage_Count (Channels) * 8 * Storage_Count (Sample_I) / SLICE_LEN +
      --                                    8 * Storage_Count (c));
      --           --  pragma Loop_Invariant (Storage_Count(Slice_End) <= Storage_Count'Last - Index);
      --
      --           --  if In_Index > Data'Last - 7 then
      --           --     End_Of_Decode := True;
      --           --     return;
      --           --  end if;
      --           Read_u64 (Slice, In_Index, Data);
      --           Scalefactor := Integer (Shift_Right (Slice, 60) and 16#f#);
      --           Slice_Start := Sample_I * Channels + c;
      --           pragma Assert (Channels > 0);
      --           pragma Assert (Clamped > 0);
      --           Slice_End := Clamped * Channels + c;
      --           pragma Assert (Slice_End > 0);
      --
      --           SI := Slice_Start;
      --           --  SI_Offset := 0;
      --
      --           --  pragma Assert ( Storage_Count(F_Samples)  * Storage_Count (Channels) <= Output'Last - Index + Storage_Count(Channels));
      --
      --           while SI < Slice_End loop
      --              Put_Line ("SI : " & SI'Img);
      --              --  pragma Loop_Invariant (Storage_Count(SI) <= Storage_Count'Last - Index);
      --              --  pragma Loop_Invariant (Storage_Count(Slice_Start) <= Output'Last - Index);
      --              --  pragma Loop_Invariant (Storage_Count(SI) >= Output'First - Index);
      --              --  pragma Loop_Invariant (SI = Slice_Start + Channels * SI_Offset);
      --
      --              --  pragma Loop_Invariant (Storage_Count(Slice_End) <= Output'Last - Index);
      --
      --              pragma Loop_invariant (SI >= 0);
      --              Predicted := Lms_Predict (Qoa.Lms (Integer (c)));
      --              Quantized := Integer (Shift_Right (Slice, 57) and 16#7#);
      --              Dequantized := DEQUANT_TAB (Scalefactor, Quantized);
      --              Reconstructed := Clamp_16 (Predicted + Dequantized);
      --              --  if Index > 2 ** 60
      --              --    or else Index + Storage_Count (SI) not in Output'Range
      --              --  then
      --              --     End_Of_Decode := True;
      --              --     return;
      --              --  end if;
      --
      --              Output (Index + Storage_Count (SI)) := Reconstructed;
      --              Slice := Shift_Left (Slice, 3);
      --              Lms_Update (Qoa.Lms (Integer (c)), Reconstructed, Dequantized);
      --              pragma Assert (Channels <= 8);
      --              --  SI_Offset := SI_Offset + 1;
      --              SI := SI + Channels;
      --           end loop;
      --        end loop;
      --        Sample_I := Sample_I + SLICE_LEN;
      --        --  Sample_I_Offset := Sample_I_Offset + 1;
      --        --  pragma Assert (Sample_I Mod SLICE_LEN = 0);
      --
      --     end loop;
      --     Frame_Len := F_Samples;
      --     End_Of_Decode := False;
      --
      --  end Decode_Frame;

            procedure Decode_Frame (Data : Storage_Array;
                              In_Index : in out Storage_Count;
                              Qoa : in out Qoa_Desc;
                              Output : in out Output_Array;
                              Index : Storage_Count;
                              Frame_Len : out Unsigned_16;
                              End_Of_Decode : out Boolean)
      is

         In_Index_Ref : constant Storage_Count := In_Index with Ghost;

         Frame_Header : Unsigned_64;
         Channels : Positive_16;
         Samplerate : Unsigned_24;
         F_Samples : Positive_16;
         Frame_Size : Positive_16;

         Data_Size : Integer;
         Num_Slices : Integer;
         Max_Total_Samples : Integer;

         History : Unsigned_64;
         Weights : Unsigned_64;

         Sample_I : Positive_16 := 0;
         Slice : Unsigned_64;
         Scalefactor : Integer;
         Slice_Start : Positive_16;
         Slice_End  : Positive_16;
         SI : Positive_16;

         Predicted : Integer;
         Quantized : Integer;
         Dequantized : Integer;
         Reconstructed : Integer_16;

         Temp_16 : Unsigned_16;
         Clamped : Positive_16;
         SI_Offset : Integer with Ghost;
         --  Sample_I_Offset : Unsigned_16 with Ghost;


      begin

         Frame_Len := 0;

         if Data'Length < 16 + LMS_LEN * 4 * Qoa.Channels then
            End_Of_Decode := True;
            return;
         end if;

         --  Read and verify the frame header
         Read_u64 (Frame_Header, In_Index, Data);
         Channels := Positive_16 (Shift_Right (Frame_Header, 56) and 16#0000ff#);
         Samplerate := Unsigned_24 (Shift_Right (Frame_Header, 32) and 16#ffffff#);
         F_Samples := Positive_16 (Shift_Right (Frame_Header, 16) and 16#00ffff#);
         Frame_Size := Positive_16 (Frame_Header and 16#00ffff#);
         if Frame_Size < 8 + LMS_LEN * 4 * Channels then
            End_Of_Decode := True;
            return;
         end if;
         Data_Size := Integer (Frame_Size) - 8 - LMS_LEN * 4 * Integer (Channels);
         Num_Slices := Data_Size / 8;
         Max_Total_Samples := Num_Slices * SLICE_LEN;

         if Storage_Count (Channels) /= Qoa.Channels or else
           Storage_Count (Samplerate) /= Qoa.Samplerate or else
           F_Samples = 0 or else
         --  Integer_64 (Frame_Size) > Data'Length or else
           In_Index > Data'Last - Storage_Count(Data_Size) or else
           F_Samples > Positive_16'Last - SLICE_LEN or else
           F_Samples > Positive_16'Last / Channels or else
           Channels * F_Samples + Channels - 1 > Positive_16'Last or else
           Storage_Count (F_Samples * Channels) > Output'Last - Index + 1 or else
           Integer (F_Samples) * Integer (Qoa.Channels) > Max_Total_Samples

         then
            End_Of_Decode := True;
            return;
         end if;
         --  pragma Assert (Index + Storage_Count(F_Samples) * Storage_Count(Channels) - 1 >= 0);
         --  Read the LMS state: 4 x 2 bytes history,
         --  4 x 2 bytes weights per channel
         for c in 0 .. Channels - 1 loop
            pragma Loop_Invariant (In_Index = In_Index_Ref + 8 + 16 * Storage_Count (c));

            Read_u64 (History, In_Index, Data);
            Read_u64 (Weights, In_Index, Data);
            for i in 0 .. LMS_LEN - 1 loop
               Temp_16 := Unsigned_16 (Shift_Right (History, 48));
               Qoa.Lms (c) .History (i) :=
                 Uns_To_Int (Temp_16);
               History := Shift_Left (History, 16);
               Temp_16 := Unsigned_16 (Shift_Right (Weights, 48));
               Qoa.Lms (c) .Weights (i) :=
                 Uns_To_Int (Temp_16);
               Weights := Shift_Left (Weights, 16);
               -- Put_Line ("Ada : predict : " & Predicted'Img & " / dequant : " & Dequantized'Img);

            end loop;
         end loop;
         Put_Line ("Channels : " & Channels'Img);
         Put_Line ("CF_Sampels : " & F_Samples'Img);
         Put_Line ("output last : " & Output'Last'Img);
         Put_Line ("Index : " & Index'Img);
         --  Decode all slices for all channels in this frame
         pragma Assert (Sample_I = 0);
         --  Sample_I_Offset := 0;
         while Sample_I < F_Samples loop
            pragma Loop_Invariant ( Channels * F_Samples + Channels - 1 <= Positive_16'Last);

            pragma Loop_Invariant (Index + Storage_Count(F_Samples) * Storage_Count (Channels) - 1 in Output'Range);
            pragma Loop_Invariant (In_Index >= Data'First);
            --  pragma Loop_Invariant (Sample_I = Sample_I_Offset * SLICE_LEN);
            --  pragma Loop_Invariant (Sample_I < Unsigned_16'Last - SLICE_LEN);
            pragma Loop_Invariant (Sample_I Mod SLICE_LEN = 0);
            pragma Loop_Invariant (In_Index = In_Index_Ref + 8 +
                                     16 * Storage_Count (Channels) +
                                     Storage_Count (Channels) * 8 * Storage_Count(Sample_I) / SLICE_LEN);
            --  pragma Loop_Invariant (Storage_Count(Slice_End) <= Storage_Count'Last - Index);
            --  pragma Loop_Invariant (Storage_Count(Slice_End) <= Output'Last - Index);
            pragma Assert (F_Samples <= Positive_16'Last - SLICE_LEN);
            Clamped := Clamp (Sample_I + SLICE_LEN, F_Samples);
            pragma Assert (Clamped <= F_Samples);
            --  pragma Assert ( Channels * Clamped + Channels - 1 <= Positive_16'Last);

            pragma Assert (Clamped > 0);
            pragma Assert (Clamped <= F_Samples);
            for c in 0 .. Channels - 1 loop
               --  pragma Loop_Invariant (Clamped <= F_Samples);
               --  pragma Loop_Invariant  (Clamped > 0  and Channels > 0);
               pragma Loop_Invariant ( Clamped <= Positive_16'Last / Channels );
               pragma Loop_Invariant ( F_Samples * Channels + c <= Positive_16'Last);
               --  pragma Loop_Invariant ( Channels * Clamped + c <= Positive_16'Last);

                --  pragma Loop_Invariant ( Channels * Sample_I + c <= Positive_16'Last);
               pragma Loop_Invariant (Index + Storage_Count (F_Samples * Channels) - 1 in Output'Range);
--  pragma Loop_Invariant (Sample_I);
               pragma Loop_Invariant (In_Index >= Data'First);
               --  pragma Loop_Invariant (In_Index = In_Index_Ref + 8 +
               --                           16 * Storage_Count (Channels) +
               --                           Storage_Count (Channels) * 8 * Storage_Count (Sample_I) / SLICE_LEN +
               --                           8 * Storage_Count (c));
               --  pragma Loop_Invariant (Storage_Count(Slice_End) <= Storage_Count'Last - Index);

               --  if In_Index > Data'Last - 7 then
               --     End_Of_Decode := True;
               --     return;
               --  end if;
               Read_u64 (Slice, In_Index, Data);
               Scalefactor := Integer (Shift_Right (Slice, 60) and 16#f#);
               --  pragma Assert (Channels <= 8 );
               --  pragma Assert (Sample_I < F_Samples);
               --  pragma Assert (c <= Channels - 1);
               --  pragma Assert ( F_Samples * Channels + c <= Positive_16'Last);
               Slice_Start := Sample_I * Channels + c;
               --  pragma Assert (Channels > 0);
               --  pragma Assert (Clamped > 0);
               --  pragma Assert (c >= 0);
               --  pragma Assert (Clamped <= F_Samples);
               --  pragma Assert (c <= Channels - 1);
               pragma Assert ( Channels * F_Samples + c <= Positive_16'Last);
               Slice_End := Clamped * Channels + c;
               pragma Assert (Slice_End >= 0);
               pragma Assert (Slice_End in Positive_16);
               SI := Slice_Start;
               SI_Offset := 0;
Put_Line (" Slice_End : " & Slice_End'Img);
               --  pragma Assert ( Storage_Count(F_Samples)  * Storage_Count (Channels) <= Output'Last - Index + Storage_Count(Channels));

               while SI < Slice_End loop
                  Put_Line ("SI : " & SI'Img);
                  --  pragma Loop_Invariant (Storage_Count(SI) <= Storage_Count'Last - Index);
                  --  pragma Loop_Invariant (Storage_Count(Slice_Start) <= Output'Last - Index);
                  --  pragma Loop_Invariant (Storage_Count(SI) >= Output'First - Index);
                  pragma Loop_Invariant (SI = Slice_Start + Channels * SI_Offset);

                  --  pragma Loop_Invariant (Storage_Count(Slice_End) <= Output'Last - Index);

                  pragma Loop_invariant (SI >= 0);
                  Predicted := Lms_Predict (Qoa.Lms (Integer (c)));
                  Quantized := Integer (Shift_Right (Slice, 57) and 16#7#);
                  Dequantized := DEQUANT_TAB (Scalefactor, Quantized);
                  Reconstructed := Clamp_16 (Predicted + Dequantized);
                  --  if Index > 2 ** 60
                  --    or else Index + Storage_Count (SI) not in Output'Range
                  --  then
                  --     End_Of_Decode := True;
                  --     return;
                  --  end if;
                  pragma Assert (Storage_Count (Slice_End - c + 1) <= Output'Last - Index + 1);
                  Output (Index + Storage_Count (SI)) := Reconstructed;
                  Slice := Shift_Left (Slice, 3);
                  Lms_Update (Qoa.Lms (Integer (c)), Reconstructed, Dequantized);
                  pragma Assert (Channels <= 8);
                  SI_Offset := SI_Offset + 1;
                  SI := SI + Channels;
               end loop;
            end loop;
            Sample_I := Sample_I + SLICE_LEN;
            --  Sample_I_Offset := Sample_I_Offset + 1;
            --  pragma Assert (Sample_I Mod SLICE_LEN = 0);

         end loop;
         Frame_Len := Unsigned_16 (F_Samples);
         End_Of_Decode := False;

      end Decode_Frame;


      Sample_Index : Unsigned_32 := 0;
      Frame_Len : Unsigned_16;
      End_Of_Decode : Boolean;
      End_Of_Header : Boolean;
      --  Data_Ref : constant Storage_Array := Data with Ghost;

   begin

      Decode_Header (Data, Qoa, End_Of_Header);
      P := Data'First + HEADER_SIZE;

      if End_Of_Header
        or else Data'Length < 8 + 16 * Qoa.Channels
        or else P > Data'Last - 7 - 16 * Qoa.Channels
or else Output'Length /= Qoa.Samples * Qoa.Channels
 then
         Output_Size := 0;
         return;
      end if;

      loop
         pragma Loop_Invariant (Data'Length >= 8 + 16 * Qoa.Channels);
         pragma Loop_Invariant (Data'Length <= Storage_Count'Last);
         pragma Loop_Invariant (P <= Data'Last - 7 - 16 * Qoa.Channels);
         pragma Loop_Invariant (P >= Data'First);
         pragma Loop_Invariant (Qoa'Initialized);

         Decode_Frame (Data, P, Qoa, Output, Index, Frame_Len, End_Of_Decode);
         Sample_Index := Sample_Index + Unsigned_32 (Frame_Len);
         pragma Assert (Qoa.Channels <= 8);
         Index := Output'First + Storage_Count (Sample_Index) * Qoa.channels;

         if End_Of_Decode or Storage_Count (Sample_Index) >= Qoa.Samples
           or P > Data'Last - 7 - 16 * Qoa.Channels
         then
            exit;
         end if;
      end loop;

      Qoa.Samples := Storage_Count (Sample_Index);

      if Index < Output'First
      then
         Output_Size := 0;
         return;
      end if;
      Output_Size := Index - Output'First;
   end Decode;

end Qoa;
