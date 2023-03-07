with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
--  with Sequential_IO; use Sequential_IO;

--  with Ada.Streams; use Ada.Streams;

--  with Audio.Wavefiles; use Audio.Wavefiles;
--  with Audio.Wavefiles.Generic_Float_PCM_IO;
--  with Audio.Wavefiles.Report;

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
--  with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
--  with Interfaces.C_Streams; use Interfaces.C_Streams;

with GNAT.OS_Lib;

with Qoa;  use Qoa;

with reference_QOA;

with AAA.Strings;

procedure Tests is

   type Storage_Array_Access is access all Storage_Array;

   type Input_Data is record
      Data : Output_Array_Access;
      Desc : Qoa.qoa_desc;
   end record;

   --  procedure Write_To_File (Filename : String;
   --                           D : Storage_Array;
   --                           Size : Storage_Count);

   --  procedure Load_WAV (Filename : String); -- return Input_Data;
   --  function Load_QOI (Filename : String) return Input_Data;

   function Img (I : Input_Data) return String
   is ("Channels:" & I.Desc.channels'Img &
         " Samplerate:" & I.Desc.samplerate'Img &
         " Samples:" & I.Desc.samples'Img &
         " Data (" & I.Data'First'Img & " .." &
         I.Data'Last'Img
       & ")");

   -------------------
   -- Write_To_File --
   -------------------

   --  procedure Write_To_File (Filename : String;
   --                           D : Storage_Array;
   --                           Size : Storage_Count)
   --  is
   --     use GNAT.OS_Lib;
   --
   --     FD : File_Descriptor;
   --     Ret : Integer;
   --  begin
   --
   --     FD := GNAT.OS_Lib.Create_File (Filename, Binary);
   --
   --     if FD = Invalid_FD then
   --        Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
   --        GNAT.OS_Lib.OS_Exit (1);
   --     end if;
   --
   --     Ret := Write (FD, D'Address, Integer (Size));
   --
   --     if Ret /= Integer (Size) then
   --        Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
   --        GNAT.OS_Lib.OS_Exit (1);
   --     end if;
   --
   --     Close (FD);
   --  end Write_To_File;

   --------------
   -- Load_WAV --
   --------------

   --  procedure Load_WAV (Filename : String) is -- return Input_Data is
   --     --  W, H, Channels_In_File : Interfaces.C.int;
   --
   --     type Float_Array is array (Positive range <>) of Float;
   --
   --     package PCM_IO is new Audio.Wavefiles.Generic_Float_PCM_IO
   --       (PCM_Sample    => Float,
   --        Channel_Range => Positive,
   --        PCM_MC_Sample => Float_Array);
   --     use PCM_IO;
   --     WF : Wavefile;
   --     --  Pixels : constant System.Address := STB.Image.Load
   --     --    (Filename, W, H, Channels_In_File, 0);
   --     --
   --     --  Len : constant Storage_Count := Storage_Count (W * H
   --     --  * Channels_In_File);
   --     --
   --     --  From_File : aliased Storage_Array (1 .. Len)
   --     --    with Address => Pixels;
   --     --
   --     --  Data : constant Storage_Array_Access := new
   --  Storage_Array (1 .. Len);
   --     --  Result : Input_Data;
   --     Last : Stream_Element_Offset;
   --     Data : Stream_Element_Array (1 .. Last);
   --  begin
   --
   --     WF.Open (In_File, Filename);
   --     if WF.Is_Open then
   --        Put_Line ("Start reading: " & Filename);
   --        New_Line;
   --        Put_Line ("Numbers of channels :"  & WF.Number_Of_Channels'Image);
   --        Put_Line ("Total count samples :"  & WF.Total_Sample_Count'Image);
   --
   --        Audio.Wavefiles.Report.Display_Info (WF);
   --
   --        Put_Line ("Current sample :  " &
   --                    Sample_Count'Image (WF.Current_Sample));
   --
   --        New_Line;
   --        Put_Line ("Finished reading "
   --      & Sample_Count'Image (WF.Total_Sample_Count) & " samples.");
   --        Put ("End time: ");
   --        Put_Line (WF.End_Time'Image);
   --
   --        Read (WF, Data);
   --
   --        WF.Close;
   --     end if;
   --
   --     --  Data.all := From_File;
   --     --
   --     --  Result.Desc := (Width      => Storage_Count (W),
   --     --                  Height     => Storage_Count (H),
   --     --                  Channels   => Storage_Count (Channels_In_File),
   --     --                  Colorspace => QOI.SRGB);
   --     --  Result.Data := Data;
   --     --  return Result;
   --  end Load_WAV;

   --------------
   -- Load_QOI --
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
      begin
         Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

         if Ret /= In_Data'Length then
            Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         Close (FD);

         Qoa.decode_header (In_Data.all, Result.Desc);

         declare
            Out_Len : constant Storage_Count :=
              Result.Desc.channels * Result.Desc.samples ;
            Out_Data : Output_Array_Access := new Output_Array (0 .. Out_Len - 1);
            Output_Size : Storage_Count;

         begin
            Put_Line ("Short_Integer Size : " &  Integer'Image (Short_Integer'Size));

            Put_Line ("Out len : " & Storage_Count'Image (Out_Len));

            Qoa.decode (data        => In_Data.all,
                        qoa        => Result.Desc,
                        Output      => Out_Data.all,
                        Output_Size => Output_Size);

            Result.Data := Out_Data;

            if reference_QOA.Check_Decode
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

   function Write_WAV (Filename : String; sample_data : Input_Data)
                       return Unsigned_32 is
      use GNAT.OS_Lib;
      data_size : constant Unsigned_32 := Unsigned_32 (sample_data.Desc.samples
       * sample_data.Desc.channels) * Unsigned_32 (Short_Integer'Size / 8) ;
      samplerate : constant Unsigned_32  :=
        Unsigned_32 (sample_data.Desc.samplerate);
      bits_per_sample : constant Unsigned_16 := 16;
      channels : constant Unsigned_16 :=
        Unsigned_16 (sample_data.Desc.channels);

      wrote : Integer;
      pragma Unreferenced (wrote);
      FD : constant File_Descriptor :=
        GNAT.OS_Lib.Create_File (Filename, Binary);

      procedure fwrite_u32 (v : Unsigned_32; FD : File_Descriptor) is
         buf : Storage_Array (0 .. 3);
         wrote : Integer;

      begin

         buf (0) := Storage_Element (16#ff# and Shift_Right (v, 0));
         buf (1) := Storage_Element (16#ff# and Shift_Right (v, 8));
         buf (2) := Storage_Element (16#ff# and Shift_Right (v, 16));
         buf (3) := Storage_Element (16#ff# and Shift_Right (v, 24));
         wrote := Write (FD, buf'Address, buf'Length);
         if wrote = 0 then
            Put_Line ("Write error");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end fwrite_u32;

      procedure fwrite_u16 (v : Unsigned_16; FD : File_Descriptor) is
         buf : Storage_Array (0 .. 1);
         wrote : Integer;
      begin
         buf (0) := Storage_Element (16#ff# and Shift_Right (v, 0));
         buf (1) := Storage_Element (16#ff# and Shift_Right (v, 8));
         wrote := Write (FD, buf'Address, buf'Length);
         if wrote = 0 then
            Put_Line ("Write error");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end fwrite_u16;

      rff : String := "RIFF";
      fmt : String := "WAVEfmt " & Character'Val(16#10#) & Character'Val(16#00#)
        & Character'Val(16#00#) & Character'Val(16#00#)
        & Character'Val(16#01#) & Character'Val(16#00#);
      data : String := "data";

   begin

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      wrote := Write (FD, rff'Address, 4);
      fwrite_u32 (data_size + 44 - 8, FD);
      wrote := Write (FD, fmt'Address, 14);
      fwrite_u16 (channels, FD);
      fwrite_u32 (samplerate, FD);
      fwrite_u32 (Unsigned_32 (channels * bits_per_sample) * samplerate / 8,
                  FD);
      fwrite_u16 (channels * bits_per_sample / 8, FD);
      fwrite_u16 (bits_per_sample, FD);
      wrote := Write (FD, data'Address, 4);
      fwrite_u32 (data_size, FD);
      wrote := Write (FD, sample_data.Data.all'Address, Integer (data_size));
      Close (FD);

      return data_size  + 44 - 8;

   end Write_WAV;

   Input : Input_Data;
begin

   if Ada.Command_Line.Argument_Count /= 2 then
      Put_Line (Standard_Error, "Usage: tests <infile> <outfile>");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   --  if AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (1), ".wav") then
   --     Put_Line ("Load WAV: " & Ada.Command_Line.Argument (1));
   --     Load_WAV (Ada.Command_Line.Argument (1)); -- Input :=
   --     els
   if  AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (1), ".qoa") then
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
--     elsif  AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (2),
--  ".qoi") then
--
--        declare
--           Output : Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
--           Output_Size : Storage_Count;
--
--        begin
--           QOI.Encode (Input.Data.all,
--                       Input.Desc,
--                       Output,
--                       Output_Size);
--
--           if Output_Size /= 0 then
--              Put_Line ("Encode: OK");
--
--              if Reference_QOI.Check_Encode
--                (Input.Data.all,
--                 Input.Desc,
--                 Output (Output'First .. Output'First + Output_Size - 1))
--              then
--                 Put_Line ("Compare with reference encoder: OK");
--              else
--                 Put_Line ("Compare with reference encoder: FAIL");
--                 GNAT.OS_Lib.OS_Exit (1);
--              end if;
--
--              Write_To_File (Ada.Command_Line.Argument (2),
--  Output, Output_Size);
--           else
--              Ada.Text_IO.Put_Line ("Encode failed");
--              GNAT.OS_Lib.OS_Exit (1);
--           end if;
--        end;

   else
      Put_Line (Standard_Error, "Invalid output file extension: '" &
                  Ada.Command_Line.Argument (2) & "'");
      GNAT.OS_Lib.OS_Exit (1);
   end if;
end Tests;
