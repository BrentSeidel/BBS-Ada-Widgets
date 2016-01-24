with Gtk.Drawing_Area;
package bbs.widget is

   type bbs_widget is abstract tagged private;

--   function attach(name : String) return bbs_widget is abstract;

   procedure failed(self : in out bbs_widget; state : Boolean);

   procedure draw(self : bbs_widget) is abstract;

private
   type bbs_widget is abstract tagged
      record
         area : Gtk.Drawing_Area.Gtk_Drawing_Area;
         failed : Boolean;
      end record;

end;
