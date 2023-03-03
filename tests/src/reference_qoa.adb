
package body reference_QOA is

   function Check_Decode (Data     : Storage_Array;
                          Out_Desc : Qoa.qoa_desc;
                          Out_Data : Storage_Array)
                             return Boolean
   is
      Ref_Output_Ptr : System.Address;
      R_Desc         : aliased Ref_Desc;
   begin
      Put_Line("Entrer");
      Ref_Output_Ptr := Decode (Data     => Data'Address,
                                Size     => Data'Length,
                                Desc     => R_Desc'Unchecked_Access);

      if Ref_Output_Ptr = System.Null_Address then
         Put_Line ("Ref QOA failed to decode");
         return False;
      end if;

      if R_Desc.channels /= unsigned (Out_Desc.channels) then
         Put_Line ("Ref channels (" & R_Desc.channels'Img & ") diff from ours "
                   & "(" & Out_Desc.channels'Img & ")");
         return False;
      end if;
      if R_Desc.samplerate /= unsigned (Out_Desc.samplerate) then
         Put_Line ("Ref samplerate (" & R_Desc.samplerate'Img &
                     ") diff from ours " & "("
                   & Out_Desc.samplerate'Img & ")");
         return False;
      end if;
      if R_Desc.samples /= unsigned (Out_Desc.samples) then
         Put_Line ("Ref samples (" & R_Desc.samples'Img &
                     ") diff from ours " &
                     "(" & Out_Desc.samples'Img & ")");
         return False;
      end if;
      --  if R_Desc.colorspace /= char'Enum_Val (Out_Desc.Colorspace'Enum_Rep)
      --  then
      --     Put_Line ("Ref colorspace (" & R_Desc.colorspace'Enum_Rep'Img &
      --                 ") diff from ours " &
      --                 "(" & Out_Desc.Colorspace'Enum_Rep'Img & ")");
      --     return False;
      --  end if;

      declare
         Ref_Out_Len : constant Storage_Count :=
           Storage_Count (R_Desc.channels *
                            R_Desc.samples *
                              Short_Integer'Size);

         Ref_Output : Storage_Array (1 .. Ref_Out_Len)
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
                  A : constant Storage_Element :=
                    Ref_Output (Ref_Output'First + Offset);
                  B : constant Storage_Element :=
                    Out_Data (Out_Data'First + Offset);
               begin
                  if A /= B then
                     Put_Line ("Byte diff" & Offset'Img &
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
end reference_QOA;
