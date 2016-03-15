package body bbs.widget.altimeter is

   function Get_Type return Glib.GType is
   begin
       Glib.Object.Initialize_Class_Record
         (Ancestor     => Gtk.Drawing_Area.Get_Type,
          Class_Record => Klass,
          Type_Name    => "bbs_altimeter",
          Signals      => Glib.Object.No_Signals,
          Parameters   => Glib.Object.Null_Parameter_Types);
      return Klass.The_Type;
   end;

   procedure gtk_new(self : in out bbs_altimeter) is
   begin
      self := new bbs_altimeter_record;
      initialize(self);
   end;

   procedure initialize(self : not null access bbs_altimeter_record'class) is
   begin
      Glib.Object.G_New(Object => self,
                        Typ    => Get_Type);
      Gtk.Drawing_Area.Initialize(self);
      self.On_Draw(draw_altimeter'access, True);
   end;
   --
   -- Attach the window and request the size as well as setting reasonable
   -- defaults for most values.
   --
   procedure setup(self : not null access bbs_altimeter_record'class;
                  radius : Float; parent : Gdk.Gdk_Window) is
   begin
      self.value := 0.0;
      self.pointer := 0.0;
      self.radius := radius;
      self.size := Integer(self.radius*2.0 + 50.0);
      self.slew := false;
      self.failed := False;
      Glib.Properties.Set_Property(self, Gtk.Widget.Height_Request_Property, Glib.Gint(self.size));
      Glib.Properties.Set_Property(self, Gtk.Widget.Width_Request_Property, Glib.Gint(self.size));
      self.Set_Window(parent);
      self.Set_Has_Window(True);
   end;

   procedure set_value(self : in out bbs_altimeter_record'Class; value : Float) is
   begin
      self.value := value;
      if (not self.slew) then
         self.pointer := self.value;
         self.Queue_Draw;
      end if;
   end;

   procedure set_failed(self : in out bbs_altimeter_record'Class; value : Boolean) is
   begin
      self.failed := value;
      self.Queue_Draw;
   end;

   procedure set_slew(self : in out bbs_altimeter_record'Class; value : Boolean; rate : Float) is
   begin
      self.slew_rate := rate;
      self.slew := value;
      if (value) then
         self.callback_id := self.Add_Tick_Callback(slew_handler'Access, null);
      else
         self.Remove_Tick_Callback(self.callback_id);
         self.pointer := self.value;
      end if;
   end;

   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean is
      me : constant bbs_altimeter := bbs_altimeter(self);
   begin
      if (me.pointer /= me.value) then
         if (me.pointer < me.value) then
            if ((me.value - me.pointer) < me.slew_rate) then
               me.pointer := me.value;
            else
               me.pointer := me.pointer + me.slew_rate;
            end if;
         else
            if ((me.pointer - me.value) < me.slew_rate) then
               me.pointer := me.value;
            else
               me.pointer := me.pointer - me.slew_rate;
            end if;
         end if;
         self.Queue_Draw;
      end if;
      return True;
   end;

   function draw_altimeter(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean is
      me : constant bbs_altimeter := bbs_altimeter(self);
      matrix : aliased Cairo.Cairo_Matrix;
   begin
      if (me.failed) then
         draw_failed(me, context);
      else
         Cairo.Translate(context, Glib.Gdouble(Float(me.size)/2.0),
                         Glib.Gdouble(Float(me.size)/2.0));
         Cairo.Get_Matrix(context, matrix'Access);
         me.draw_altimeter_arc(context);
         me.draw_ticks(context);
         Cairo.Set_Matrix(context, matrix'Access);
         draw_pointer(me, context);
      end if;
      return True;
   end;

   procedure draw_altimeter_arc(Self : access bbs_altimeter_record'Class; context : Cairo.Cairo_Context) is
      start : Glib.Gdouble;
      stop : Glib.Gdouble;
   begin
      start := Glib.Gdouble(0.0);
      stop := Glib.Gdouble(Ada.Numerics.Pi*2.0);
      Cairo.Set_Line_Width(context, 4.0);
      Cairo.Set_Source_Rgb(context, 0.0, 0.0, 0.0);
      Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius), start, stop);
      Cairo.Stroke(context);
   end;

   procedure draw_ticks(Self : access bbs_altimeter_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
      ticks : Integer;
   begin
      Cairo.Set_Source_Rgb(context, 0.0, 0.0, 0.0);
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi));
      Cairo.Get_Matrix(context, matrix'Access);
      --
      -- First draw the major ticks
      --
      ticks := 10;
      Cairo.Set_Line_Width(context, 2.0);
      for x in 0 .. (ticks - 1) loop
         Cairo.Set_Matrix(context, matrix'Access);
         Cairo.Rotate(context, Glib.Gdouble(float(x)*two_pi/float(ticks)));
         Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 10.0));
         Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius + 5.0));
         Cairo.Move_To(context, -5.0, Glib.Gdouble(self.radius + 15.0));
         Cairo.Show_Text(context, Integer'Image(integer(float(x))));
      end loop;
      Cairo.Stroke(context);
      --
      -- Then draw the minor ticks
      --
      ticks := 20;
      Cairo.Set_Line_Width(context, 1.0);
      for x in 0 .. (ticks - 1) loop
         Cairo.Set_Matrix(context, matrix'Access);
         Cairo.Rotate(context, Glib.Gdouble(float(x)*two_pi/float(ticks)));
         Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
         Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
      end loop;
      Cairo.Stroke(context);
   end;
   --
   -- The altimeter actually has three pointers.  The large pointer gives the
   -- altitude mod 100 feet, the small pointer gives the altitude mod 1000 feet,
   -- and the triangle gives the altitude mod 10000 feet.
   --
   procedure draw_pointer(Self : access bbs_altimeter_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
      triangle : constant float := self.pointer;
      small : constant float := float(integer(self.pointer) mod 10000);
      big : constant float := float(integer(self.pointer) mod 1000);
   begin
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi));
      Cairo.Set_Source_Rgb(context, 0.0, 0.0, 0.0);
      Cairo.Set_Line_Width(context, 2.0);
      Cairo.Get_Matrix(context, matrix'Access);
--      Ada.Text_IO.Put_Line("Altimeter value tri: " & Integer'Image(Integer(triangle)) &
--                             ", small: "  & Integer'Image(Integer(small)) &
--                             ", big: "  & Integer'Image(Integer(big)));
      --
      -- Draw triangle
      --
      Cairo.Get_Matrix(context, matrix'Access);
--      Cairo.Arc(context, 0.0, 0.0, 20.0, 0.0, Glib.Gdouble(two_pi));
      Cairo.Rotate(context, Glib.Gdouble(triangle*two_pi/100000.0));
      Cairo.Move_To(context, -10.0, 20.0);
      Cairo.Line_To(context, 0.0, 30.0);
      Cairo.Line_To(context, 10.0, 20.0);
      Cairo.Line_To(context, -10.0, 20.0);
      Cairo.Fill(context);
      --
      -- Draw small pointer
      --
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, Glib.Gdouble(small*two_pi/10000.0));
      Cairo.Move_To(context, 0.0, -10.0);
      Cairo.Line_To(context, 5.0, 0.0);
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius - 30.0));
      Cairo.Line_To(context, -5.0, 0.0);
      Cairo.Line_To(context, 0.0, -10.0);
      Cairo.Fill(context);
      --
      -- Draw large pointer
      --
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, Glib.Gdouble(big*two_pi/1000.0));
      Cairo.Move_To(context, 0.0, -10.0);
      Cairo.Line_To(context, 5.0, 0.0);
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius - 10.0));
      Cairo.Line_To(context, -5.0, 0.0);
      Cairo.Line_To(context, 0.0, -10.0);
      Cairo.Fill(context);
   end;
   --
   -- This function draws a red X on the widget area.  It is used when data for
   -- the widget is failed.
   --
   procedure draw_failed(Self : access bbs_altimeter_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Line_Width(context, 5.0);
      Cairo.Set_Source_Rgb(context, 1.0, 0.0, 0.0);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), Glib.Gdouble(Float(self.size)));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(Float(self.size)));
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), 0.0);
      Cairo.Stroke(context);
   end;

--   function compute_angle(Self : access bbs_altimeter_record'Class; value : Float) return Float is
--   begin
--      return 3.0*Ada.Numerics.Pi/2.0 + value*two_pi/(self.max - self.min);
--   end;

end;
