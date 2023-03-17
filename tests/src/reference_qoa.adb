with Ada.Text_IO; use Ada.Text_IO;
package body Reference_QOA is

   function Check_Encode (Samples   : Output_Array;
                          Desc   : Qoa.Qoa_Desc;
                          Output : Storage_Array)
                          return Boolean
   is
      Ref_Output_Ptr : System.Address;
      R_Desc         : aliased Ref_Desc;
      Ref_Out_Len    : aliased Interfaces.C.int;
   begin
      R_Desc.Channels      := unsigned (Desc.Channels);
      R_Desc.Samplerate     := unsigned (Desc.Samplerate);
      R_Desc.Samples   := unsigned (Desc.Samples);

      Ref_Output_Ptr := Encode (Data    => Samples'Address,
                                Desc    => R_Desc'Unrestricted_Access,
                                Out_Len => Ref_Out_Len'Access);

      if Ref_Output_Ptr = System.Null_Address then
         Put_Line ("Ref QOA failed to encode");
         return False;
      end if;

      declare
         Ref_Output : Storage_Array (1 .. Storage_Count (Ref_Out_Len))
           with Address => Ref_Output_Ptr;

         To_Compare : constant Storage_Count :=
           Storage_Count'Min (Ref_Output'Length, Output'Length);
      begin
         if Ref_Output /= Output then
            if Ref_Output'Length > Output'Length then
               Put_Line ("Ref QOA output bigger (" &
                           Ref_Output'Length'Img & ")" &
                           " than ours (" & Output'Length'Img & ")");

            elsif Ref_Output'Length < Output'Length then
               Put_Line ("Ref QOA output smaller (" &
                           Ref_Output'Length'Img & ")" &
                           " than ours (" & Output'Length'Img & ")");
            end if;

            for Offset in 0 .. To_Compare - 1 loop
               declare
                  A : constant Storage_Element :=
                    Ref_Output (Ref_Output'First + Offset);
                  B : constant Storage_Element :=
                    Output (Output'First + Offset);
               begin
                  Put_Line ("cmp " & Offset'Img &
                              " A : " & A'Img &
                              "   B : " & B'Img);
                  --  if A /= B then
                  --     Put_Line ("Samples diff" & Offset'Img &
                  --                 " Ref:" & A'Img &
                  --                 " Act:" & B'Img);
                  --     return False;
                  --  end if;
               end;
            end loop;

            return False;
         end if;
      end;

      return True;
   end Check_Encode;

   function Check_Decode (Data     : Storage_Array;
                          Out_Desc : Qoa.Qoa_Desc;
                          Out_Data : Output_Array)
                          return Boolean
   is
      Ref_Output_Ptr : System.Address;
      R_Desc         : aliased Ref_Desc;
   begin
      Ref_Output_Ptr := Decode (Data     => Data'Address,
                                Size     => Data'Length,
                                Desc     => R_Desc'Unchecked_Access);

      if Ref_Output_Ptr = System.Null_Address then
         Put_Line ("Ref QOA failed to decode");
         return False;
      end if;

      if R_Desc.Channels /= unsigned (Out_Desc.Channels) then
         Put_Line ("Ref channels (" & R_Desc.Channels'Img & ") diff from ours "
                   & "(" & Out_Desc.Channels'Img & ")");
         return False;
      end if;
      if R_Desc.Samplerate /= unsigned (Out_Desc.Samplerate) then
         Put_Line ("Ref samplerate (" & R_Desc.Samplerate'Img &
                     ") diff from ours " & "("
                   & Out_Desc.Samplerate'Img & ")");
         return False;
      end if;
      if R_Desc.Samples /= unsigned (Out_Desc.Samples) then
         Put_Line ("Ref samples (" & R_Desc.Samples'Img &
                     ") diff from ours " &
                     "(" & Out_Desc.Samples'Img & ")");
         return False;
      end if;

      declare
         Ref_Out_Len : constant Storage_Count :=
           Storage_Count (R_Desc.Channels *
                            R_Desc.Samples);

         Ref_Output : Output_Array (1 .. Ref_Out_Len)
           with Address => Ref_Output_Ptr;

         To_Compare : constant Storage_Count :=
           Storage_Count'Min (Ref_Output'Length, Out_Data'Length);
      begin
         if Ref_Output /= Out_Data then
            if Ref_Output'Length > Out_Data'Length then
               Put_Line ("Ref QOA decode output bigger (" &
                           Ref_Output'Length'Img & ")" &
                           " than ours (" & Out_Data'Length'Img & ")");

            elsif Ref_Output'Length < Out_Data'Length then
               Put_Line ("Ref QOA decode output smaller (" &
                           Ref_Output'Length'Img & ")" &
                           " than ours (" & Out_Data'Length'Img & ")");
            end if;

            for Offset in 0 .. To_Compare - 1 loop
               declare
                  A : constant Integer_16 :=
                    Ref_Output (Ref_Output'First + Offset);
                  B : constant Integer_16 :=
                    Out_Data (Out_Data'First + Offset);
               begin
                  if A /= B then
                     Put_Line ("Sample diff" & Offset'Img &
                                 " Ref:" & A'Img &
                                 " Act:" & B'Img);
                     return False;
                  end if;
               end;
            end loop;

            return False;
         end if;
      end;

      return True;
   end Check_Decode;
end Reference_QOA;
