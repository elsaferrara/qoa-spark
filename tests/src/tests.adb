with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with Interfaces; use Interfaces;

with GNAT.OS_Lib;

with Qoa;  use Qoa;
with Reference_QOA;

with AAA.Strings;

procedure Tests is

   type Storage_Array_Access is access all Storage_Array;

   type Input_Data is record
      Data : Output_Array_Access;
      Desc : Qoa.Qoa_Desc;
   end record;

   function Img (I : Input_Data) return String
   is ("Channels:" & I.Desc.Channels'Img &
         " Samplerate:" & I.Desc.Samplerate'Img &
         " Samples:" & I.Desc.Samples'Img &
         " Data (" & I.Data'First'Img & " .." &
         I.Data'Last'Img
       & ")");

   -------------------
   -- Write_To_File --
   -------------------

   procedure Write_To_File (Filename : String;
                            D : Storage_Array;
                            Size : Storage_Count)
   is
      use GNAT.OS_Lib;

      FD : File_Descriptor;
      Ret : Integer;
   begin

      FD := GNAT.OS_Lib.Create_File (Filename, Binary);

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Ret := Write (FD, D'Address, Integer (Size));

      if Ret /= Integer (Size) then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Close (FD);
   end Write_To_File;

   --------------
   -- Load_WAV --
   --------------

   function Load_WAV (Filename : String)
   return Input_Data is
      use GNAT.OS_Lib;

      FD : File_Descriptor;
      GR : Integer;
      pragma Unreferenced (GR);

      function Read_u32 (FD : File_Descriptor) return Unsigned_32 is
         Buf : Storage_Array (0 .. 3);
         Good_Read : Integer;
         pragma Unreferenced (Good_Read);
      begin
         Good_Read := Read (FD, Buf'Address, 4);
         return Shift_Left (Unsigned_32 (Buf (3)), 24)
                             or Shift_Left (Unsigned_32 (Buf (2)), 16)
                             or Shift_Left (Unsigned_32 (Buf (1)), 8)
                             or Unsigned_32 (Buf (0));

      end Read_u32;

      function Read_u16 (FD : File_Descriptor) return Unsigned_16 is
         Buf : Storage_Array (0 .. 1);
         Good_Read : Integer;
         pragma Unreferenced (Good_Read);
      begin
         Good_Read := Read (FD, Buf'Address, 2);
         return Unsigned_16 ((Buf (1) * (2 ** 8))
                             or Buf (0));
      end Read_u16;

      Container_Type : String (1 .. 4);
      Type_Ref : constant String := "RIFF";

      Wavid : String (1 .. 4);
      Wav_Size : Unsigned_32;
      pragma Unreferenced (Wav_Size);
      Wav_Ref : constant String := "WAVE";

      Chunk_Type : String (1 .. 4);
      Chunk_Size : Unsigned_32;
      Fmt_Ref : constant String := "fmt ";

      Data_Size : Unsigned_32 := 0;
      Format_Type : Unsigned_16 := 0;
      Channels : Unsigned_16 := 0;
      Samplerate : Unsigned_32 := 0;
      Byte_Rate : Unsigned_32 := 0;
      pragma Unreferenced (Byte_Rate);
      Block_Align : Unsigned_16 := 0;
      pragma Unreferenced (Block_Align);
      Bits_Per_Sample : Unsigned_16 := 0;
      Data_Ref : constant String := "data";

   begin
      FD := GNAT.OS_Lib.Open_Read (Filename, Binary);

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (Standard_Error, GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      GR := Read (FD, Container_Type'Address, 4);
      if Container_Type /= Type_Ref then
         Put_Line ("Not a RIFF container");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Wav_Size := Read_u32 (FD);
      GR := Read (FD, Wavid'Address, 4);

      if Wavid /= Wav_Ref then
         Put_Line ("No WAVE id found");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      while True loop
         GR := Read (FD, Chunk_Type'Address, 4);
         GR := Read (FD, Chunk_Size'Address, 4);

         if Chunk_Type = Fmt_Ref then
            if Chunk_Size /= 16 then
               Put_Line ("WAV fmt chunk size missmatch");
               GNAT.OS_Lib.OS_Exit (1);
            end if;
            Format_Type := Read_u16 (FD);
            Channels := Read_u16 (FD);
            Samplerate := Read_u32 (FD);
            Byte_Rate := Read_u32 (FD);
            Block_Align := Read_u16 (FD);
            Bits_Per_Sample := Read_u16 (FD);

         elsif Chunk_Type = Data_Ref then
            Data_Size := Chunk_Size;
            exit;

         else
            Lseek (FD, Long_Integer (Chunk_Size), Seek_Cur);
         end if;
      end loop;

      if Format_Type /= 1 then
         Put_Line ("Type in fmt chunk is not PCM");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      if Bits_Per_Sample /= 16 then
         Put_Line ("Bits per samples != 16");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      if Data_Size = 0 then
         Put_Line ("No data chunk");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      declare

         Len : constant Storage_Count := Storage_Count (Data_Size / 2);
         Data : constant Output_Array_Access :=
           new Output_Array (0 .. Len - 1);
         Good_Read : constant Integer :=
           Read (FD, Data.all'Address, Integer (Data_Size));
         Result : Input_Data;

      begin

         Put_Line ("good read : " & Good_Read'Img);
         Result.Desc := (Channels   => Storage_Count (Channels),
                         Samplerate     => Storage_Count (Samplerate),
                         Samples   => Storage_Count (Data_Size /
                           (Unsigned_32 (Channels * (Bits_Per_Sample / 8)))),
                         Lms => <>);

         Result.Data := Data;
         Close (FD);

         return Result;
      end;
   end Load_WAV;

   --------------
   -- Load_QOA --
   --------------

   function Load_QOA (Filename : String) return Input_Data is
      use GNAT.OS_Lib;

      FD : File_Descriptor;
      Ret : Integer;
      Result : Input_Data;
   begin

      FD := GNAT.OS_Lib.Open_Read (Filename, Binary);

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (Standard_Error, GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      declare
         Len : constant Storage_Count := Storage_Count (File_Length (FD));
         In_Data : constant Storage_Array_Access :=
           new Storage_Array (1 .. Len);
         End_Of_Header : Boolean;
      begin

         Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

         if Ret /= In_Data'Length then
            Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         Close (FD);

         Qoa.Decode_Header (In_Data.all, Result.Desc, End_Of_Header);

         declare
            Out_Len : constant Storage_Count :=
              Result.Desc.Channels * Result.Desc.Samples;
            Out_Data : constant Output_Array_Access :=
              new Output_Array (0 .. Out_Len - 1);
            Output_Size : Storage_Count;

         begin
            Put_Line ("Short_Integer Size : " &
                        Integer'Image (Short_Integer'Size));

            Put_Line ("Out len : " & Storage_Count'Image (Out_Len));

            Qoa.Decode (Data        => In_Data.all,
                        Qoa        => Result.Desc,
                        Output      => Out_Data.all,
                        Output_Size => Output_Size);

            Result.Data := Out_Data;

            if Reference_QOA.Check_Decode
              (In_Data.all,
               Result.Desc,
               Out_Data.all (Out_Data'First .. Out_Data'First +
                     Output_Size - 1))
            then
               Put_Line ("Compare with reference decoder: OK");
            else
               Put_Line ("Compare with reference decoder: FAIL");
               GNAT.OS_Lib.OS_Exit (1);
            end if;
            Put_Line ("TRUE");
            return Result;
         end;
      end;
   end Load_QOA;

   function Write_WAV (Filename : String; Sample_Data : Input_Data)
                       return Unsigned_32 is
      use GNAT.OS_Lib;
      Data_Size : constant Unsigned_32 := Unsigned_32 (Sample_Data.Desc.Samples
       * Sample_Data.Desc.Channels) * Unsigned_32 (Short_Integer'Size / 8);
      Samplerate : constant Unsigned_32  :=
        Unsigned_32 (Sample_Data.Desc.Samplerate);
      Bits_Per_Sample : constant Unsigned_16 := 16;
      Channels : constant Unsigned_16 :=
        Unsigned_16 (Sample_Data.Desc.Channels);

      Wrote : Integer;
      pragma Unreferenced (Wrote);
      FD : constant File_Descriptor :=
        GNAT.OS_Lib.Create_File (Filename, Binary);

      procedure Write_u32 (V : Unsigned_32; FD : File_Descriptor) is
         Buf : Storage_Array (0 .. 3);
         Wrote : Integer;
      begin
         Buf (0) := Storage_Element (16#ff# and Shift_Right (V, 0));
         Buf (1) := Storage_Element (16#ff# and Shift_Right (V, 8));
         Buf (2) := Storage_Element (16#ff# and Shift_Right (V, 16));
         Buf (3) := Storage_Element (16#ff# and Shift_Right (V, 24));
         Wrote := Write (FD, Buf'Address, Buf'Length);
         if Wrote = 0 then
            Put_Line ("Write error");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end Write_u32;

      procedure Write_u16 (V : Unsigned_16; FD : File_Descriptor) is
         Buf : Storage_Array (0 .. 1);
         Wrote : Integer;
      begin
         Buf (0) := Storage_Element (16#ff# and Shift_Right (V, 0));
         Buf (1) := Storage_Element (16#ff# and Shift_Right (V, 8));
         Wrote := Write (FD, Buf'Address, Buf'Length);
         if Wrote = 0 then
            Put_Line ("Write error");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end Write_u16;

      Rff : String := "RIFF";
      Fmt : String := "WAVEfmt "
        & Character'Val (16#10#) & Character'Val (16#00#)
        & Character'Val (16#00#) & Character'Val (16#00#)
        & Character'Val (16#01#) & Character'Val (16#00#);
      Data_String : String := "data";

   begin

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      Wrote := Write (FD, Rff'Address, 4);
      Write_u32 (Data_Size + 44 - 8, FD);
      Wrote := Write (FD, Fmt'Address, 14);
      Write_u16 (Channels, FD);
      Write_u32 (Samplerate, FD);
      Write_u32 (Unsigned_32 (Channels * Bits_Per_Sample) * Samplerate / 8,
                  FD);
      Write_u16 (Channels * Bits_Per_Sample / 8, FD);
      Write_u16 (Bits_Per_Sample, FD);
      Wrote := Write (FD, Data_String'Address, 4);
      Write_u32 (Data_Size, FD);
      Wrote := Write (FD, Sample_Data.Data.all'Address, Integer (Data_Size));
      Close (FD);

      return Data_Size  + 44 - 8;

   end Write_WAV;

   Input : Input_Data;
begin

   if Ada.Command_Line.Argument_Count /= 2 then
      Put_Line (Standard_Error, "Usage: tests <infile> <outfile>");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (1), ".wav") then
      Put_Line ("Load WAV: " & Ada.Command_Line.Argument (1));
      Input := Load_WAV (Ada.Command_Line.Argument (1));

   elsif  AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (1), ".qoa") then
      Put_Line ("Load QOA: " & Ada.Command_Line.Argument (1));
      Input := Load_QOA (Ada.Command_Line.Argument (1));
   else
      Put_Line (Standard_Error, "Invalid input file extension: '" &
                  Ada.Command_Line.Argument (1) & "'");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   Put_Line ("Loaded -> " & Img (Input));

   if AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (2), ".wav") then

      declare
         Result : Unsigned_32;
      begin
         Result := Write_WAV (Ada.Command_Line.Argument (2),
                                        Input);

         if Result = 0 then
            Put_Line (Standard_Error, "WAV write error: '" &
                        Ada.Command_Line.Argument (2) & "'");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end;
   elsif  AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (2), ".qoa") then

      declare
         Output : Storage_Array
           (1 .. Qoa.Encode_Size (Input.Desc.Channels, Input.Desc.Samples));
         Output_Size : Storage_Count;

      begin
         Qoa.Encode (Input.Data.all,
                     Input.Desc,
                     Output,
                     Output_Size);

         if Output_Size /= 0 then
            Put_Line ("Encode: OK");

            if Reference_QOA.Check_Encode
              (Input.Data.all,
               Input.Desc,
               Output (Output'First .. Output'First + Output_Size - 1))
            then
               Put_Line ("Compare with reference encoder: OK");
            else
               Put_Line ("Compare with reference encoder: FAIL");
               GNAT.OS_Lib.OS_Exit (1);
            end if;

            Write_To_File (Ada.Command_Line.Argument (2),
            Output, Output_Size);
         else
            Ada.Text_IO.Put_Line ("Encode failed");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end;

   else
      Put_Line (Standard_Error, "Invalid output file extension: '" &
                  Ada.Command_Line.Argument (2) & "'");
      GNAT.OS_Lib.OS_Exit (1);
   end if;
end Tests;
