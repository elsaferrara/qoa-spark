
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with Interfaces.C; use Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with Qoa;

package reference_QOA is

   type Ref_Desc is record
      channels      : Interfaces.C.unsigned;
      samplerate    : Interfaces.C.unsigned;
      samples   : Interfaces.C.unsigned;
      lms : Qoa.Array_lms;
   end record
     with Convention => C;

   type Ref_Desc_Acc is access all Ref_Desc;

   function Check_Decode (Data     : Storage_Array;
                          Out_Desc : Qoa.qoa_desc;
                          Out_Data : Storage_Array)
                          return Boolean;

   function Decode (Data     : System.Address;
                    Size     : Interfaces.C.int;
                    Desc     : not null Ref_Desc_Acc)
                    return System.Address;
   pragma Import (C, Decode, "qoa_decode");

end reference_QOA;
