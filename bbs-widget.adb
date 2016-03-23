with Interfaces.C.Strings;
with Glib;
use type Glib.Gdouble;
package body bbs.widget is
   --
   procedure set_color(c : Cairo.Cairo_Context; color : RGB_color) is
   begin
      Cairo.Set_Source_Rgb(c, Glib.Gdouble(color.R),
                           Glib.Gdouble(color.G), Glib.Gdouble(color.B));
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
