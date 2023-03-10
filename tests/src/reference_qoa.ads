
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with Interfaces.C; use Interfaces.C;

with Qoa; use Qoa;
with Interfaces; use Interfaces;

package Reference_QOA is

   type Ref_Desc is record
      Channels      : Interfaces.C.unsigned;
      Samplerate    : Interfaces.C.unsigned;
      Samples   : Interfaces.C.unsigned;
      Lms : Qoa.Array_lms;
   end record
     with Convention => C;

   type Ref_Desc_Acc is access all Ref_Desc;

         function Check_Encode (Samples   : Output_Array;
                          Desc   : Qoa.Qoa_Desc;
                          Output : Storage_Array)
                                return Boolean;

      function Encode (Data     : System.Address;
                    Desc     : not null Ref_Desc_Acc;
                    Out_Len  : not null access Interfaces.C.int)
                    return System.Address;
   pragma Import (C, Encode, "qoa_encode");

   function Check_Decode (Data     : Storage_Array;
                          Out_Desc : Qoa.Qoa_Desc;
                          Out_Data : Output_Array)
                          return Boolean;

   function Decode (Data     : System.Address;
                    Size     : Interfaces.C.int;
                    Desc     : not null Ref_Desc_Acc)
                    return System.Address;
   pragma Import (C, Decode, "qoa_decode");

end Reference_QOA;
