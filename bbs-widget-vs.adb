package body bbs.widget.vs is

   function Get_Type return Glib.GType is
   begin
       Glib.Object.Initialize_Class_Record
         (Ancestor     => Gtk.Drawing_Area.Get_Type,
          Class_Record => Klass,
          Type_Name    => "bbs_vs",
          Signals      => Glib.Object.No_Signals,
          Parameters   => Glib.Object.Null_Parameter_Types);
      return Klass.The_Type;
   end;

   procedure gtk_new(self : in out bbs_vs) is
   begin
      self := new bbs_vs_record;
      initialize(self);
   end;

   procedure initialize(self : not null access bbs_vs_record'class) is
   begin
      Glib.Object.G_New(Object => self,
                        Typ    => Get_Type);
      Gtk.Drawing_Area.Initialize(self);
      self.On_Draw(draw_vs'access, True);
   end;
   --
   -- Attach the window and request the size as well as setting reasonable
   -- defaults for most values.
   --
   procedure setup(self : not null access bbs_vs_record'class;
                  width : Integer; parent : Gdk.Gdk_Window) is
   begin
      self.min := -2000.0;
      self.max := 2000.0;
      self.value := 0.0;
      self.pointer := 0.0;
      self.major := 8;
      self.minor := 32;
      self.radius := Float(width - 50)/2.0;
      self.size := width;
      self.slew := false;
      self.failed := False;
      self.arc_start := 3.0*Ada.Numerics.Pi/2.0 + 0.2;
      self.arc_end := 3.0*Ada.Numerics.Pi/2.0 - 0.2;
      Glib.Properties.Set_Property(self, Gtk.Widget.Height_Request_Property, Glib.Gint(self.size));
      Glib.Properties.Set_Property(self, Gtk.Widget.Width_Request_Property, Glib.Gint(self.size));
      self.Set_Window(parent);
      self.Set_Has_Window(True);
   end;
   --
   procedure set_value(self : in out bbs_vs_record'Class; value : Float) is
   begin
      self.value := value;
      if (value < self.min) then
         self.value := self.min;
      end if;
      if (value > self.max) then
         self.value := self.max;
      end if;
      if (not self.slew) then
         self.pointer := self.value;
         self.Queue_Draw;
      end if;
   end;
   --
   procedure set_failed(self : in out bbs_vs_record'Class; value : Boolean) is
   begin
      self.failed := value;
      self.Queue_Draw;
   end;
   --
   procedure set_slew(self : in out bbs_vs_record'Class; value : Boolean; rate : Float) is
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
   --
   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean is
      me : constant bbs_vs := bbs_vs(self);
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
   --
   function draw_vs(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean is
      me : constant bbs_vs := bbs_vs(self);
      matrix : aliased Cairo.Cairo_Matrix;
   begin
      if (me.failed) then
         draw_failed(me, context);
      else
         Cairo.Translate(context, Glib.Gdouble(Float(me.size)/2.0),
                         Glib.Gdouble(Float(me.size)/2.0));
         Cairo.Scale(context, Glib.Gdouble(float(me.size) / 300.0),
                     Glib.Gdouble(float(me.size) / 300.0));
         Cairo.Get_Matrix(context, matrix'Access);
         me.draw_background(context);
--         me.draw_ticks(context);
         Cairo.Set_Matrix(context, matrix'Access);
         draw_pointer(me, context);
      end if;
      return True;
   end;

   procedure draw_background(Self : access bbs_vs_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
      ticks : Integer;
   begin
      set_color(context, color_black);
      Cairo.Paint(context);
      Cairo.Set_Line_Width(context, 4.0);
      set_color(context, color_white);
      Cairo.Arc(context, 0.0, 0.0, 125.0,
                Glib.Gdouble(self.arc_start + Ada.Numerics.Pi/2.0),
                Glib.Gdouble(self.arc_end + Ada.Numerics.Pi/2.0));
      Cairo.Stroke(context);
      --
      Cairo.Get_Matrix(context, matrix'Access);
      --
      -- First draw the major ticks
      --
      set_color(context, color_white);
      ticks := self.major;
      Cairo.Set_Line_Width(context, 2.0);
      Cairo.Set_Font_Size(context, 12.0);
      for x in 0 .. ticks loop
         Cairo.Set_Matrix(context, matrix'Access);
         Cairo.Rotate(context, Glib.Gdouble(self.arc_start + float(x)*(two_pi - 0.4)/float(self.major)));
         Cairo.Move_To(context, 0.0, 110.0);
         Cairo.Line_To(context, 0.0, 125.0);
         center_text(context, self.radius + 15.0,
                     Integer'Image(integer(self.min + float(x)*(self.max - self.min)/float(self.major))/100));
      end loop;
      Cairo.Stroke(context);
      --
      -- Then draw the minor ticks
      --
      ticks := self.minor;
      Cairo.Set_Line_Width(context, 1.0);
      for x in 0 .. ticks loop
         Cairo.Set_Matrix(context, matrix'Access);
         Cairo.Rotate(context, Glib.Gdouble(self.arc_start + float(x)*(two_pi - 0.4)/float(self.minor)));
         Cairo.Move_To(context, 0.0, 115.0);
         Cairo.Line_To(context, 0.0, 125.0);
      end loop;
      Cairo.Stroke(context);
      --
      -- Other text labels
      --
      Cairo.Set_Matrix(context, matrix'Access);
      center_text(context, -20.0, "VERTICAL SPEED");
      Cairo.Set_Font_Size(context, 9.0);
      center_text(context, 30.0, "100 FEET PER MIN");
      Cairo.Move_To(context, -110.0, -20.0);
      Cairo.Show_Text(context, "UP");
      Cairo.Move_To(context, -110.0, 30.0);
      Cairo.Show_Text(context, "DN");
   end;
   --
   procedure draw_pointer(Self : access bbs_vs_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Line_Width(context, 2.0);
      Cairo.Rotate(context, Glib.Gdouble(self.arc_start + (self.pointer - self.min)*(two_pi - 0.4)/(self.max - self.min)));
      set_color(context, color_white);
      Cairo.Move_To(context, 0.0, -10.0);
      Cairo.Line_To(context, 5.0, 0.0);
      Cairo.Line_To(context, 0.0, 110.0);
      Cairo.Line_To(context, -5.0, 0.0);
      Cairo.Line_To(context, 0.0, -10.0);
      Cairo.Fill(context);
   end;

   --
   -- This function draws a red X on the widget area.  It is used when data for
   -- the widget is failed.
   --
   procedure draw_failed(Self : access bbs_vs_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Line_Width(context, 5.0);
      set_color(context, color_fail);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), Glib.Gdouble(Float(self.size)));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(Float(self.size)));
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), 0.0);
      Cairo.Stroke(context);
   end;

   function compute_angle(Self : access bbs_vs_record'Class; value : Float) return Float is
   begin
      return Ada.Numerics.Pi/2.0 + self.arc_start + value*(self.arc_end - self.arc_start)/(self.max - self.min);
   end;

end;
