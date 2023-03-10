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

   type Qoa_Desc is record
      Channels : Storage_Count; -- Unsigned_8
      Samplerate : Storage_Count; -- Unsigned_24
      Samples : Storage_Count; -- Unsigned_16
      Lms : Array_lms;
   end record;

   type array_short is array (Integer range <>) of Integer_16;



   function Encode_Worst_Case (Qoa : Qoa_Desc) return Storage_Count;

   procedure Encode (Sample_Data :     Output_Array;
                     Qoa         : in out Qoa_Desc;
                     Output      : out Storage_Array;
                     Output_len  : out Storage_Count);

   procedure Decode_Header (Data : Storage_Array;
                            Qoa : out Qoa_Desc);

   procedure Decode (Data        :     Storage_Array;
                     Qoa         : out Qoa_Desc;
                     Output      : out Output_Array;
                     Output_Size : out Storage_Count);


private



   QUANT_TAB : constant array (-8 .. 8) of Integer := (7, 7, 7, 5, 5, 3, 3,
                                                       1, 0, 0, 2, 2, 4, 4,
                                                       6, 6, 6);
   SCALEFACTOR_TAB : constant array (0 .. 15) of Integer :=
     (1, 7, 21, 45, 84, 138, 211, 304, 421,
     562, 731, 928, 1157, 1419, 1715, 2048);
   RECIPROCAL_TAB : constant array (0 .. 15) of Integer :=
     (65536, 9363, 3121, 1457, 781, 475, 311,
      216, 156, 117, 90, 71, 57, 47, 39, 32);
   DEQUANT_TAB : constant array (0 .. 15, 0 .. 7) of Integer :=
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

   function Lms_Predict (Lms : qoa_lms_t) return Integer;


   procedure Lms_Update (Lms : in out qoa_lms_t;
                         Sample : Integer;
                         Residual : Integer);

   function Div (V : Integer;
                 Scalefactor : Integer)
                 return Integer;

   function Clamp (V : Integer;
                       Min : Integer;
                       Max : Integer)
                       return Integer;


end Qoa;
