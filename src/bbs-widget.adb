--
-- This file is copyrighted (C) 2016 by Brent Seidel.  It is available under
-- version 3 of the GPL.  See the file LICENSE for more details.
--
-- Please contact the author if you are interested in other licensing arrangements.
--
with Interfaces.C.Strings;
with Glib;
use type Glib.Gdouble;
package body bbs.widget is
   --
   procedure set_color(c : Cairo.Cairo_Context; color : RGB_color) is
   begin
      Cairo.Set_Source_Rgb(c, color.R, color.G, color.B);
   end;
   --
   procedure center_text(c : Cairo.Cairo_Context; y : Float; text : String) is
      ex : aliased Cairo.Cairo_Text_Extents;
   begin
      Cairo.Text_Extents(c, Interfaces.C.Strings.New_String(text), ex'Access);
      Cairo.Move_To(c, -ex.Width/2.0, Glib.Gdouble(y));
      Cairo.Show_Text(c, text);
   end;

end;
