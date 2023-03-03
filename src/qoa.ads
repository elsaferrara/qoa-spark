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
   FRAME_LEN : constant := (SLICES_PER_FRAME * SLICE_LEN);
   LMS_LEN : constant := 4;
   MAGIC : constant := 16#716f6166#; -- 'qoaf'
   HEADER_SIZE : constant := 5;


   type my_array is array (1 .. LMS_LEN) of Integer_16;
   type qoa_lms_t is record
      history : my_array;
      weights : my_array;
   end record;

   type Array_lms is array (1 .. MAX_CHANNELS) of qoa_lms_t;

   type qoa_desc is record
      channels : Storage_Count; -- Unsigned_8
      samplerate : Storage_Count; -- Unsigned_24
      samples : Storage_Count; -- Unsigned_16
      lms : Array_lms;
   end record;

   type array_short is array (Integer range <>) of Integer_16;
   --  procedure encode (sample_data :     array_short;
   --                        qoa         : in out qoa_desc;
   --                        Output      : out Storage_Array;
   --                        Output_len  : out Storage_Count);


   procedure decode_header (data : Storage_Array;
                            qoa : out qoa_desc);

   procedure decode (data        :     Storage_Array;
                     qoa        : out qoa_desc;
                     Output      : out Storage_Array;
                     Output_Size : out Storage_Count)
     --  with
     --    Pre => Output'First >= 0
     --    and then Output'Last < Storage_Count'Last
     --    and then Data'First >= 0
     --    and then Data'Last < Storage_Count'Last
     --    and then Data'Length >= MIN_FILESIZE

   ;



   --     function decode_header (data : Storage_Array;
   --                              qoa : out qoa_desc)
   --  return Integer;

private



   QUANT_TAB : constant array (-8 .. 8) of Integer := (7, 7, 7, 5, 5, 3, 3,
                                                       1, 0, 0, 2, 2, 4, 4,
                                                       6, 6, 6);
   SCALEFACTOR_TAB : constant array (1 .. 16) of Integer :=
     (1, 7, 21, 45, 84, 138, 211, 304, 421,
     562, 731, 928, 1157, 1419, 1715, 2048);
   RECIPROCAL_TAB : constant array (1 .. 16) of Integer :=
     (65536, 9363, 3121, 1457, 781, 475, 311,
      216, 156, 117, 90, 71, 57, 47, 39, 32);
   DEQUANT_TAB : constant array (1 .. 16, 1 .. 8) of Integer :=
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

   function lms_predict (lms : qoa_lms_t) return Integer;


   procedure lms_update (lms : in out qoa_lms_t;
                         sample : Integer;
                         residual : Integer)
     with
       Pre => residual / (2 ** 4) in Integer
   ;

   --  function qoa_div (v : Integer;
   --                    scalefactor : Integer)
   --                    return Integer;
   --
   function clamp (v : Integer;
                       min : Integer;
                       max : Integer)
                       return Integer;
   --
   --  function Read_u64 (data : Storage_Array; p : Storage_Count;
   --                    Res : out Unsigned_64)
   --                     return Storage_Count;
   --  procedure write_u64 (val : Unsigned_64; bytes : in out Storage_Array;
   --                        index : in out Storage_Offset);

end Qoa;
