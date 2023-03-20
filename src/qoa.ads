with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

package Qoa
  with SPARK_Mode
is

      -- QOA tags --

   MIN_FILESIZE : constant := 16;
   MAX_CHANNELS : constant := 8;
   SLICE_LEN : constant := 20;
   SLICES_PER_FRAME : constant := 256;
   GEN_FRAME_LEN : constant := (SLICES_PER_FRAME * SLICE_LEN);
   LMS_LEN : constant := 4;
   MAGIC : constant := 16#716f6166#; -- 'qoaf'
   HEADER_SIZE : constant := 8;


   type Output_Array is array (Storage_Count range <>) of Interfaces.Integer_16;
   type Output_Array_Access is access all Output_Array;

   type My_Array is array (0 .. LMS_LEN - 1) of Integer_16;
   type Qoa_Lms_t is record
      History : My_Array := (others => 0);
      Weights : My_Array := (others => 0);
   end record;

   type Array_lms is array (0 .. MAX_CHANNELS - 1) of Qoa_Lms_t;

   subtype Channels_Type is Storage_Count range 1 .. MAX_CHANNELS;

   type Qoa_Desc is record
      Channels : Channels_Type; -- Unsigned_8
      Samplerate : Storage_Count; -- Unsigned_24
      Samples : Storage_Count; -- Unsigned_16
      Lms : Array_lms;
   end record;

   type array_short is array (Integer range <>) of Integer_16;



   function Encode_Worst_Case (Qoa : Qoa_Desc) return Storage_Count
     with
       Pre => Qoa.Samples <= 2 ** 32
       and then (Qoa.Samples + SLICE_LEN - 1)
         / SLICE_LEN < Storage_Count'Last / 256,
     Post => Encode_Worst_Case'Result >= 8
   ;

   procedure Encode (Sample_Data :     Output_Array;
                     Qoa         : in out Qoa_Desc;
                     Output      : out Storage_Array;
                     Output_len  : out Storage_Count)
     with
       Relaxed_Initialization => Output,
         Pre => Output'First >= 0
         and then Output'Last < Storage_Count'Last
       and then Qoa.Samples <= 2 ** 32
         and then (Qoa.Samples + SLICE_LEN - 1) / SLICE_LEN < Storage_Count'Last / 256
       and then Output'Length >= Encode_Worst_Case (Qoa)
       and then Sample_Data'Last < Storage_Count'Last
         and then Sample_Data'Length < Storage_Count'Last
   ;

   procedure Decode_Header (Data : Storage_Array;
                            Qoa : out Qoa_Desc;
                           End_Of_Header : out Boolean)
     with
       Relaxed_Initialization => Qoa,
       Pre => Data'First >= 0
         and then Data'Last < Storage_Count'Last
       and then Data'Length >= HEADER_SIZE + 8,
       Post =>
         (if End_Of_Header = False then
            Qoa'Initialized
         )

   ;

   procedure Decode (Data        :     Storage_Array;
                     Qoa         : out Qoa_Desc;
                     Output      : out Output_Array;
                     Output_Size : out Storage_Count)
     with
       Relaxed_Initialization => (Output, Qoa),
       Pre => Output'First >= 0
         and then Output'Last < Storage_Count'Last
         and then Data'First >= 0
         and then Data'Last < Storage_Count'Last
         and then Data'Length >= HEADER_SIZE + 8
   ;



private


   subtype Quant_Int is Integer range 0 .. 7;
   QUANT_TAB : constant array (-8 .. 8) of Quant_Int := (7, 7, 7, 5, 5, 3, 3,
                                                       1, 0, 0, 2, 2, 4, 4,
                                                       6, 6, 6);
   SCALEFACTOR_TAB : constant array (0 .. 15) of Integer :=
     (1, 7, 21, 45, 84, 138, 211, 304, 421,
     562, 731, 928, 1157, 1419, 1715, 2048);
   RECIPROCAL_TAB : constant array (0 .. 15) of Integer :=
     (65536, 9363, 3121, 1457, 781, 475, 311,
      216, 156, 117, 90, 71, 57, 47, 39, 32);

   subtype Dequant_Int is Integer range -14336 .. 14336;
   DEQUANT_TAB : constant array (0 .. 15, 0 .. 7) of Dequant_Int :=
   ((1,    -1,    3,    -3,    5,    -5,     7,     -7),
   (5,    -5,   18,   -18,   32,   -32,    49,    -49),
   (16,   -16,   53,   -53,   95,   -95,   147,   -147),
   (34,   -34,  113,  -113,  203,  -203,   315,   -315),
   (63,   -63,  210,  -210,  378,  -378,   588,   -588),
   (104,  -104,  345,  -345,  621,  -621,   966,   -966),
   (158,  -158,  528,  -528,  950,  -950,  1477,  -1477),
   (228,  -228,  760,  -760, 1368, -1368,  2128,  -2128),
   (316,  -316, 1053, -1053, 1895, -1895,  2947,  -2947),
   (422,  -422, 1405, -1405, 2529, -2529,  3934,  -3934),
   (548,  -548, 1828, -1828, 3290, -3290,  5117,  -5117),
   (696,  -696, 2320, -2320, 4176, -4176,  6496,  -6496),
   (868,  -868, 2893, -2893, 5207, -5207,  8099,  -8099),
   (1064, -1064, 3548, -3548, 6386, -6386,  9933,  -9933),
    (1286, -1286, 4288, -4288, 7718, -7718, 12005, -12005),
    (1536, -1536, 5120, -5120, 9216, -9216, 14336, -14336));

   procedure Read_u64 (Res : out Unsigned_64; P : in out Storage_Count;
                       Data : Storage_Array)
     with
       Pre => P >= Data'First
       and then  Data'Length >= 8
       and then P <= Data'Last - 7
       and then P <= Storage_Count'Last - 8,
       Post => P = P'Old + 8
       and then P >= Data'First
   ;
      procedure Write_u64 (Val : Unsigned_64; Bytes : in out Storage_Array;
                           Index : in out Storage_Count)
     with
              Pre => Index >= Bytes'First
       and then  Bytes'Length >= 8
       and then Index <= Bytes'Last - 7
         and then Index <= Storage_Count'Last - 8,
       Post => Index = Index'Old + 8
         and then Bytes'Length >= 8

     ;

   function Lms_Predict (Lms : qoa_lms_t) return Integer
     with
   Post => Abs (Lms_Predict'Result) < 2 ** 24;


   procedure Lms_Update (Lms : in out qoa_lms_t;
                         Sample : Integer_16;
                         Residual : Integer)
   ;

   function Div (V : Integer;
                 Scalefactor : Integer)
                 return Integer
     with
       Pre =>  Scalefactor in RECIPROCAL_TAB'Range
   ;

   function Clamp (V : Integer;
                   Min : Integer;
                   Max : Integer)
                   return Integer
     with
       Post => Clamp'Result in Min .. Max
   ;

   function Clamp_16 (V : Integer)
                            return Integer_16;

   --  function Valid_Size ( Qoa : Qoa_Desc) return Boolean is
   --    (Qoa.Channels in 1 .. Storage_Count (Integer_8'Last)
   --     and then Qoa.Samplerate in 1 .. Storage_Count (Integer_24'Last)
   --     and then Qoa.Samples in 1 .. Storage_Count (Integer_16'Last)
   --         and then 8 +
   --       Num_Frames * 8 +
   --         Num_Frames * LMS_LEN * 4 * Qoa.Channels +
   --       Num_Slices * 8 * Qoa.Channels
   --
   --    )
   --
   --     ;


end Qoa;
