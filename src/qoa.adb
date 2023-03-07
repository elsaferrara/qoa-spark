with Unchecked_Conversion;
package body Qoa
  with SPARK_Mode
is
   function uns_to_int is new Unchecked_Conversion (Unsigned_64, Integer_16);

   --  function Read_u64 (Arr : Storage_Array; Index : in out Storage_Offet)
   --  return Unsigned_64 is
   --     res : Unsigned_64;
   --  begin
   --     res := Arr(Index) * (2 ** 56) or Arr(Index + 1) * (2 ** 56)
   --
   --       Index := Index + 8;
   --     return res;
   --  end Read_u64;

   function clamp (v : Integer;
                   min : Integer;
                   max : Integer)
                         return Integer
   is
   begin
      if v < min then
         return min;
      else
         if v > max then
            return max;
         else
            return v;
         end if;
      end if;
   end clamp;


   function Frame_Size (channels : Unsigned_32; slices : Unsigned_32)
     return Unsigned_32
   is
   begin
      return 8 + LMS_LEN * 4 * channels + 8 * slices * channels;
   end Frame_Size;
   --
   --  procedure write_u64 (val : Unsigned_64; bytes : in out Storage_Array;
   --                       index : in out Storage_Offset)
   --  is
   --  begin
   --     bytes (index) := Storage_Element ((val / (2 ** 56)) and 16#ff#);
   --     bytes (index + 1) := Storage_Element ((val / (2 ** 48)) and 16#ff#);
   --     bytes (index + 2) := Storage_Element ((val / (2 ** 40)) and 16#ff#);
   --     bytes (index + 3) := Storage_Element ((val / (2 ** 32)) and 16#ff#);
   --     bytes (index + 4) := Storage_Element ((val / (2 ** 24)) and 16#ff#);
   --     bytes (index + 5) := Storage_Element ((val / (2 ** 6)) and 16#ff#);
   --     bytes (index + 6) := Storage_Element ((val / (2 ** 8)) and 16#ff#);
   --     bytes (index + 7) := Storage_Element ((val / (2 ** 0)) and 16#ff#);
   --     index := index + 8;
   --  end write_u64;
   --
   --------------
   -- Prediction --
   --------------


   function lms_predict (lms : qoa_lms_t) return Integer is
      prediction : Integer_64 := 0;
   begin
      for i in 0 .. LMS_LEN - 1 loop
          prediction := prediction +
           Integer_64 (lms.weights (i) * lms.history (i));
         Put_Line ("Ada : prediction " & i'Img & " : " & prediction'Img);
         null;
      end loop;
      return Integer ((prediction - (prediction mod 2**13))/ 2**13); -- Shift_Right
   end lms_predict;
   --
   --  ------------
   --   Update ----
   --  ------------

   procedure lms_update (lms : in out qoa_lms_t;
                         sample : Integer;
                         residual : Integer)
   is
      d : Integer_16 := Integer_16 ((residual - (residual mod 2**4))/ 2**4);
   begin
      --  if residual < 0
      --  then
      --     d := Integer_16 (residual / (2 ** 4)) - 1;
      --  else
      --     d := Integer_16 (residual / (2 ** 4));
      --  end if;
      --
         --     for i in 0 .. LMS_LEN - 1 loop
         --  Put_Line ("Ada : lms.weights " & i'Img & " : " & lms.weights (i)'Img);
         --  Put_Line ("Ada : lms.history " & i'Img & " : " & lms.history (i)'Img);
         --  end loop;
      Put_Line ("Ada : delta : " & d'Img);
      for i in 0 .. LMS_LEN - 1 loop
         if lms.history (i) < 0 then
            lms.weights (i) := lms.weights (i) - d;
         else
            lms.weights (i) := lms.weights (i) +  d;
         end if;

      end loop;
      for i in 0 .. LMS_LEN - 2 loop
         lms.history (i) := lms.history (i + 1);
      end loop;
      lms.history (LMS_LEN - 1) :=  Integer_16 (sample);
      --  for i in 0 .. LMS_LEN - 1 loop
      --     Put_Line ("Ada : lms.weights " & i'Img & " : " & lms.weights (i)'Img);
      --     Put_Line ("Ada : lms.history " & i'Img & " : " & lms.history (i)'Img);
      --     end loop;
   end lms_update;


   procedure Read_u64 (Res : out Unsigned_64; p : in out Storage_Count;
                       data : Storage_Array)
      is
         r0 : constant Unsigned_64 := Unsigned_64 (data (p));
         r1 : constant Unsigned_64 := Unsigned_64 (data (p + 1));
         r2 : constant Unsigned_64 := Unsigned_64 (data (p + 2));
         r3 : constant Unsigned_64 := Unsigned_64 (data (p + 3));
         r4 : constant Unsigned_64 := Unsigned_64 (data (p + 4));
         r5 : constant Unsigned_64 := Unsigned_64 (data (p + 5));
         r6 : constant Unsigned_64 := Unsigned_64 (data (p + 6));
         r7 : constant Unsigned_64 := Unsigned_64 (data (p + 7));
      begin
         Res := Shift_Left (r0, 56)
           or Shift_Left (r1, 48)
           or Shift_Left (r2, 40)
           or Shift_Left (r3, 32)
           or Shift_Left (r4, 24)
           or Shift_Left (r5, 16)
           or Shift_Left (r6, 8)
           or Shift_Left (r7, 0);
         --  Put_Line ("Ada : Res : " & Res'Img);
         --  Put_Line ("Ada : p : " & p'Img);
         p := p + 8;
      end Read_u64;

   --
   --  function div (v : Integer;
   --                scalefactor : Integer)
   --                      return Integer
   --  is
   --     reciprocal : constant Integer := RECIPROCAL_TAB (scalefactor);
   --  n : constant Integer := (v * reciprocal + (1 * (2 ** 15))) / (2 ** 16);
   --  begin
   --     if v > 0 and then n < 0 then
   --        return n + 2;
   --     elsif v < 0 and then n > 0 then
   --        return n - 2;
   --     else
   --        return n;
   --     end if;
   --  end div;
   --
   --  procedure encode (sample_data :     array_short;
   --                    qoa         :     in out qoa_desc;
   --                        Output      : out Storage_Array;
   --                        Output_len  : out Storage_Count)
   --  is
   --
   --     function encode_header (qoa : qoa_desc; bytes : Storage_Array)
   --                             return Storage_Offset
   --     is
   --        p : Storage_Offset := bytes'First;
   --     begin
   --        write_u64 ((Unsigned_64 (MAGIC)) * (2 ** 32) or
   --                     (Unsigned_64 (qoa.samples)), Output, p);
   --        return p;
   --     end encode_header;
   --
   --     function encode_frame (sample_data : array_short;
   --                            qoa : in out qoa_desc;
   --                            bytes : in out Storage_Array;
   --                            index : in out Storage_Offset)
   --                            return Storage_Offset
   --     is
   --        channels : constant Unsigned_32 := qoa.channels;
   --        slices : constant Integer := (sample_data'Length + SLICE_LEN - 1)
   --          / SLICE_LEN;
   --        f_size : constant Unsigned_32 :=
   --          Frame_Size (channels, Unsigned_32 (slices));
   --        weights : Unsigned_64;
   --        history : Unsigned_64;
   --        sample_index : Integer := 0;
   --
   --        slice_size : Integer;
   --        slice_start : Integer;
   --        slice_end : Integer;
   --
   --        best_error : Unsigned_64;
   --        best_slice : Unsigned_64;
   --        best_lms : qoa_lms_t;
   --
   --        lms : qoa_lms_t;
   --        slice : Unsigned_64;
   --        current_error : Unsigned_64;
   --        si : Integer;
   --
   --        sample : Integer;
   --        predicted : Integer;
   --        residual : Integer;
   --        scaled : Integer;
   --        clamped : Integer;
   --        quantized : Integer;
   --        dequantized : Integer;
   --        reconstructed : Integer;
   --        error : Integer_64;
   --
   --     begin
   --  Write the frame header
   --        write_u64 ((
   --                      Unsigned_64 (qoa.channels) * (2 ** 56) +
   --                        Unsigned_64 (qoa.samplerate) * (2 ** 32) +
   --                        Unsigned_64 (sample_data'Length) * (2 ** 16) +
   --                        Unsigned_64 (f_size)
   --                     ), bytes, index);
   --
   --  Write the current LMS state
   --        for c in Integer range 1 .. (Integer (channels)) loop
   --           weights := 0;
   --           history := 0;
   --           for i in 1 .. LMS_LEN loop
   --              history := (history * (2 ** 16)) or
   --                (Unsigned_64 (qoa.lms (c) .history (i)) and 16#ffff#);
   --              weights := (weights * (2 ** 16)) or
   --                (Unsigned_64 (qoa.lms (c) .weights (i)) and 16#ffff#);
   --           end loop;
   --           write_u64 (history, bytes, index);
   --           write_u64 (weights, bytes, index);
   --        end loop;
   --
   --  Encode all samples
   --        while sample_index < sample_data'Length loop
   --
   --           for c in Integer range 1 .. Integer (channels) loop
   --              slice_size := clamp (SLICE_LEN, 0,
   --                                   sample_data'Length - sample_index);
   --              slice_start := sample_index * Integer (channels) + c;
   --              slice_end := (sample_index + slice_size)
   --                * Integer (channels) + c;
   --
   --  Search the best scalefactor. Go through all 16 scalefactors,
   --  encode all samples for the current slice and
   --  measure the total squared error.
   --              best_error := -1;
   --
   --              for scalefactor in 1 .. 16 loop
   --
   --  Reset the LMS state to the last known good one
   --  before trying each scalefactor
   --
   --                 lms := qoa.lms (c);
   --                 slice := Unsigned_64 (scalefactor);
   --                 current_error := 0;
   --
   --                 si := slice_start;
   --                 while si < slice_end loop
   --                    sample := Integer (sample_data (si));
   --                    predicted := lms_predict (lms);
   --
   --                    residual := sample - predicted;
   --                    scaled := div (residual, scalefactor);
   --                    clamped := clamp (scaled, -8, 8);
   --                    quantized := QUANT_TAB (clamped + 8);
   --                    dequantized := DEQUANT_TAB (scalefactor, quantized);
   --                    reconstructed := clamp (predicted + dequantized,
   --                                            -32768, 32767);
   --
   --                    error := Integer_64 (sample - reconstructed);
   --                    current_error := current_error +
   --                      Unsigned_64 (error * error);
   --                    if current_error > best_error then
   --                       exit;
   --                    end if;
   --
   --                    lms_update (lms, reconstructed, dequantized);
   --       slice := (slice * (2 ** 3)) or Unsigned_64 (quantized);
   --
   --                    si := si + Integer (channels);
   --                 end loop;
   --
   --                 if current_error < best_error then
   --                    best_error := current_error;
   --                    best_slice := slice;
   --                    best_lms := lms;
   --                 end if;
   --              end loop;
   --              qoa.lms (c) := best_lms;
   --
   --              best_slice := best_slice *
   --                (2 ** ((SLICE_LEN - slice_size) * 3));
   --              write_u64 (best_slice, bytes, index);
   --
   --           end loop;
   --           sample_index := sample_index + SLICE_LEN;
   --        end loop;
   --        return index;
   --     end encode_frame;
   --
   --  num_frames : Unsigned_32 := (qoa.samples + FRAME_LEN - 1) / FRAME_LEN;
   --  num_slices : Unsigned_32 := (qoa.samples + SLICE_LEN - 1) / SLICE_LEN;
   --     encoded_size : Unsigned_32 := 8 +
   --                              num_frames * 8 +
   --                              num_frames * LMS_LEN * 4 * qoa.channels +
   --                              num_slices * 8 * qoa.channels;
   --
   --     index : Storage_Offset;
   --     f_len : Integer := FRAME_LEN;
   --     sample_index : Integer := 0;
   --     frame_samples : Integer;
   --     frame_size : Unsigned_32;
   --  begin
   --
   --  initialize LMS weights to {0, 0, -1, 2}
   --     for c in Integer range 1 .. Integer (qoa.channels) loop
   --        qoa.lms (c) .weights (1) := 0;
   --        qoa.lms (c) .weights (2) := 0;
   --        qoa.lms (c) .weights (3) := -1 * (2 ** 13);
   --        qoa.lms (c) .weights (4) :=  1 * (2 ** 14);
   --
   --  initialize LMS weights to {0, 0, 0, 0}
   --        for i in 0 .. LMS_LEN loop
   --           qoa.lms (c) .history (i) := 0;
   --        end loop;
   --     end loop;
   --
   --  Encode the header and go through all frames
   --     index := encode_header (qoa, Output);
   --
   --     while sample_index < Integer (qoa.samples) loop
   --  f_len := clamp(FRAME_LEN, 0, qoa.samples - sample_index);
   --  frame_samples := Output_len + sample_index * Integer (qoa.channels);
   --  frame_size := encode_frame(frame_samples, qoa,
   --  sample_data'Length, output, index);
   --        index := index + frame_size;
   --        sample_index := sample_index + sample_data'Length;
   --     end loop;
   --  end encode;




      procedure decode_header (data : Storage_Array;
                               qoa : out qoa_desc)
      is

       in_index : Storage_Count:= data'First ;
         file_header : Unsigned_64;
         frame_header : Unsigned_64;


   begin

      if data'Length < MIN_FILESIZE then
         return;
      end if;

      Read_u64 (file_header, in_index, data);
      if Shift_Right (file_header, 32) /= MAGIC then
         return;
      end if;

      qoa.samples := Storage_Count (file_header and 16#ffffffff#);


      Read_u64 (frame_header, in_index, data);
      qoa.channels :=
        Storage_Count (Shift_Right (frame_header, 56) and 16#0000ff#);
      qoa.samplerate :=
        Storage_Count (Shift_Right (frame_header, 32) and 16#ffffff#);

      if qoa.channels = 0 or else qoa.samples = 0
        or else qoa.samplerate = 0
      then
         return;
      end if;

      Put_Line ("qoa channels init : " & Storage_Count'Image (qoa.channels ));
      Put_Line ("qoa samplerate init : " & Storage_Count'Image (qoa.samplerate));
      Put_Line ("qoa samples init : " & Storage_Count'Image (qoa.samples ));
      Put_Line ("p : " & Storage_Count'Image (in_index));
      Put_Line ("data first : " & Storage_Count'Image (data'First ));
      Put_Line ("header size : " & Integer'Image (HEADER_SIZE ));



      end decode_header;

   procedure decode (data        :     Storage_Array;
                     qoa        : out qoa_desc;
                     Output      : out Output_Array;
                     Output_Size : out Storage_Count)
   is
      p : Storage_Count;
      index : Storage_Count := Output'First;



       procedure decode_frame (data : Storage_Array;
                             in_index : in out Storage_Count;
                             qoa : in out qoa_desc;
                             Output : in out Output_Array;
                              index : Storage_Count;
                               frame_len : out Unsigned_32;
                              End_Of_Decode : out Boolean)
      is

         frame_header : Unsigned_64;
         channels : Integer;
         samplerate : Integer;
         fsamples : Integer;
         frame_size : Integer;

         data_size : Integer;
         num_slices : Integer;
         max_total_samples : Integer;

         history : Unsigned_64;
         weights : Unsigned_64;

         sample_i : Integer := 0;
         slice : Unsigned_64;
         scalefactor : Integer;
         slice_start : Integer;
         slice_end : Integer;
         si : Integer;

         predicted : Integer;
         quantized : Integer;
         dequantized : Integer;
         reconstructed : Integer;


      begin

         if data'Length < 16 + LMS_LEN * 4 * qoa.channels then
            End_Of_Decode := True;
            return;     --  return 0;
         end if;

         --  Read and verify the frame header
         Read_u64 (frame_header, in_index, data);
         channels := Integer (Shift_Right (frame_header, 56) and 16#0000ff#);
         samplerate := Integer (Shift_Right (frame_header, 32) and 16#ffffff#);
         fsamples    := Integer (Shift_Right (frame_header, 16) and 16#00ffff#);
         frame_size := Integer ((frame_header) and 16#00ffff#);

         --  Put_Line ("num channels: " & Integer'Image (channels));
         --  Put_Line ("sampelrate: " & Integer'Image (samplerate));
         --  Put_Line ("fsamples: " & Integer'Image (fsamples));
         --  Put_Line ("f_size: " & Integer'Image (frame_size));



         data_size := frame_size - 8 - LMS_LEN * 4 * channels;
         num_slices := data_size / 8;
         max_total_samples := num_slices * SLICE_LEN;

         if channels /= Integer (qoa.channels) or else
           samplerate /= Integer (qoa.samplerate) or else
           frame_size > data'Length or else
           fsamples * channels > max_total_samples

         then
            End_Of_Decode := True;
            return;     --  return 0;
         end if;

         --  Read the LMS state: 4 x 2 bytes history,
         --  4 x 2 bytes weights per channel
         for  c in 0 .. channels - 1 loop
            Read_u64 (history, in_index, data);
            Read_u64 (weights, in_index, data);
            for i in 0 .. LMS_LEN - 1 loop
               qoa.lms (c) .history (i) :=
                 uns_to_int (Shift_Right (history, 48));

               history := Shift_Left (history, 16);
               qoa.lms (c) .weights (i) :=
                 uns_to_int (Shift_Right (weights, 48));
               weights := Shift_Left (weights, 16);

            end loop;
         end loop;

         --  Decode all slices for all channels in this frame


         while sample_i < fsamples loop

            for c in 0 .. channels - 1 loop
               Read_u64 (slice, in_index, data);
               scalefactor := Integer (Shift_Right (slice, 60) and 16#f#);
               slice_start := sample_i * channels + c;
               slice_end := clamp (sample_i + SLICE_LEN, 0, fsamples)
                 * channels + c;
               --  Put_Line ("fsamples : " & fsamples'Img);
               --  Put_Line ("sice end : " & slice_end'Img);
               --                 Put_Line ("index : " & index'Img);
               --
               si := slice_start;
               while si < slice_end loop

                  predicted := lms_predict (qoa.lms (c));
                  quantized := Integer (Shift_Right (slice, 57) and 16#7#);
                  dequantized := DEQUANT_TAB (scalefactor, quantized);
                  reconstructed := clamp (predicted + dequantized,
                                          -32768, 32767);
                  --  if index + Storage_Count (si) > 5000 then
                  --  Put_Line ("indice: " & Storage_Count'Image (index + Storage_Count (si)));
                  --  end if;
                     --  Put_Line ("indice 1 : " & Output'First'Img);
                     --  Put_Line ("indice last : " & Output'Last'Img);
                  Put_Line ("Ada : predicted: " & predicted'Img);
                  Put_Line ("Ada : quantized: " & quantized'Img);
                  Put_Line ("Ada : dequantized: " & dequantized'Img);
                  Put_Line ("Ada : reconstructed: " & reconstructed'Img);

                  Output (index + Storage_Count (si)) := Integer_16 (reconstructed);

                  slice := Shift_Left (slice, 3);
                  lms_update (qoa.lms (c), reconstructed, dequantized);
                  si := si + channels;
               end loop;
            end loop;
            sample_i := sample_i + SLICE_LEN;
         end loop;

         frame_len := Unsigned_32 (fsamples);
         End_Of_Decode := False;

      end decode_frame;

      sample_index : Unsigned_32 := 0;
      frame_len : Unsigned_32;
       End_Of_Decode : Boolean;

   begin

      --  if decode_header (data, qoa) = 0 then
      --     Output_Size := 0;
      --     return;
      --  end if;
      p := data'First;
      decode_header (data, qoa);
       p := data'First + HEADER_SIZE;
      Put_Line ("data first: " & data'First'Img);

      loop

         index := Storage_Count (sample_index) * qoa.channels;
         decode_frame (data, p, qoa, Output, index,frame_len, End_Of_Decode);
         sample_index := sample_index + frame_len;
         Put_Line ("sample index : " & sample_index'Img);
         if End_Of_Decode or sample_index >= Unsigned_32 (qoa.samples) then
            exit;
         end if;
      end loop;
      Put_Line ("qoa samples : " & Storage_Count'Image (qoa.samples));
      qoa.samples := Storage_Count (sample_index);
      Put_Line ("qoa samplesbis : " & Storage_Count'Image (qoa.samples));

      index := Storage_Count (sample_index) * qoa.channels;
      -- index := qoa.samples * qoa.channels;
       Put_Line ("final index : " & Storage_Count'Image (index));

      Output_Size := index - Output'First;
      Put_Line ("Output size : " & Storage_Count'Image (Output_Size ));

   end decode;

end Qoa;
