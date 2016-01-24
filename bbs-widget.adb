package body bbs.widget is

   procedure failed(self : in out bbs_widget; state : Boolean) is
   begin
      self.failed := state;
   end;


end;
