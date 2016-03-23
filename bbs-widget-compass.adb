package body bbs.widget.compass is

   function Get_Type return Glib.GType is
   begin
       Glib.Object.Initialize_Class_Record
         (Ancestor     => Gtk.Drawing_Area.Get_Type,
          Class_Record => Klass,
          Type_Name    => "bbs_compass",
          Signals      => Glib.Object.No_Signals,
          Parameters   => Glib.Object.Null_Parameter_Types);
      return Klass.The_Type;
   end;
   --
   procedure gtk_new(self : in out bbs_compass) is
   begin
      self := new bbs_compass_record;
      initialize(self);
   end;
   --
   procedure initialize(self : not null access bbs_compass_record'class) is
   begin
      Glib.Object.G_New(Object => self,
                        Typ    => Get_Type);
      Gtk.Drawing_Area.Initialize(self);
      self.On_Draw(draw_compass'access, True);
   end;
   --
   -- Attach the window and request the size as well as setting reasonable
   -- defaults for most values.
   --
   procedure setup(self : not null access bbs_compass_record'class;
                  width : Integer; parent : Gdk.Gdk_Window) is
   begin
      self.value := 0.0;
      self.pointer := 0.0;
      self.bug := 0.0;
      self.radius := Float(width - 50)/2.0;
      self.size := width;
      self.slew := false;
      self.failed := False;
      self.bug_state := False;
      Glib.Properties.Set_Property(self, Gtk.Widget.Height_Request_Property, Glib.Gint(self.size));
      Glib.Properties.Set_Property(self, Gtk.Widget.Width_Request_Property, Glib.Gint(self.size));
      self.Set_Window(parent);
      self.Set_Has_Window(True);
   end;
   --
   procedure set_value(self : in out bbs_compass_record'Class; value : Float) is
   begin
      self.value := value;
      if (not self.slew) then
         self.pointer := self.value;
         self.Queue_Draw;
      end if;
   end;

   procedure set_failed(self : in out bbs_compass_record'Class; value : Boolean) is
   begin
      self.failed := value;
      self.Queue_Draw;
   end;
   --
   procedure set_slew(self : in out bbs_compass_record'Class; value : Boolean; rate : Float) is
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
   procedure set_bug(self : in out bbs_compass_record'Class; state : Boolean; value : Float) is
   begin
      self.bug := value;
      self.bug_state := state;
      self.Queue_Draw;
   end;
   --
   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean is
      me : constant bbs_compass := bbs_compass(self);
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

   function draw_compass(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean is
      me : constant bbs_compass := bbs_compass(self);
      matrix : aliased Cairo.Cairo_Matrix;
   begin
      if (me.failed) then
         draw_failed(me, context);
      else
         Cairo.Translate(context, Glib.Gdouble(Float(me.size)/2.0),
                         Glib.Gdouble(Float(me.size)/2.0));
         Cairo.Get_Matrix(context, matrix'Access);
         me.draw_background(context);
         Cairo.Set_Matrix(context, matrix'Access);

         draw_pointers(me, context);
      end if;
      return True;
   end;
   --
   -- The compass is a little different than other dial display widgets in that
   -- the background moves while the foreground is fixed.
   --
   procedure draw_background(Self : access bbs_compass_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
      font_mat : aliased Cairo.Cairo_Matrix;
      ticks : Integer;
   begin
      set_color(context, color_black);
      Cairo.Paint(context);
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi - self.pointer*Ada.Numerics.Pi/180.0));
      Cairo.Get_Matrix(context, matrix'Access);
      Cairo.Get_Font_Matrix(context, font_mat'Access);
      --
      -- Draw circle
      --
      Cairo.Set_Line_Width(context, 4.0);
      set_color(context, color_white);
      Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius), 0.0, Glib.Gdouble(two_pi));
      Cairo.Stroke(context);
      --
      -- First draw the major ticks
      --
      ticks := 36;
      set_color(context, color_white);
      Cairo.Set_Line_Width(context, 2.0);
      for x in 0 .. (ticks - 1) loop
         Cairo.Set_Matrix(context, matrix'Access);
         Cairo.Rotate(context, Glib.Gdouble(float(x)*two_pi/float(ticks)));
         Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 10.0));
         Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius + 5.0));
      end loop;
      Cairo.Stroke(context);
      --
      -- Then draw the minor ticks
      --
      ticks := 72;
      set_color(context, color_grey9);
      Cairo.Set_Line_Width(context, 1.0);
      for x in 0 .. (ticks - 1) loop
         Cairo.Set_Matrix(context, matrix'Access);
         Cairo.Rotate(context, Glib.Gdouble(float(x)*two_pi/float(ticks)));
         Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
         Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
      end loop;
      Cairo.Stroke(context);
      --
      -- Draw the heading bug, if needed
      --
      if (self.bug_state) then
         set_color(context, color_grey5);
         Cairo.Set_Line_Width(context, 1.0);
         Cairo.Set_Matrix(context, matrix'Access);
         Cairo.Rotate(context, Glib.Gdouble(self.bug*Ada.Numerics.Pi/180.0));
         Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius));
         Cairo.Line_To(context, -10.0, Glib.Gdouble(self.radius + 15.0));
         Cairo.Line_To(context, 10.0, Glib.Gdouble(self.radius + 15.0));
         Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
         Cairo.Fill(context);
      end if;
      --
      -- Draw the labels
      --
      ticks := 12;
      Cairo.Set_Font_Size(context, 12.0);
      set_color(context, color_white);
      for x in 0 .. (ticks - 1) loop
         Cairo.Set_Matrix(context, matrix'Access);
         Cairo.Rotate(context, Glib.Gdouble(float(x)*two_pi/float(ticks)));
         case x is
            when 0 =>
               center_text(context, self.radius + 15.0, "N");
            when 3 =>
               center_text(context, self.radius + 15.0, "E");
            when 6 =>
               center_text(context, self.radius + 15.0, "S");
            when 9 =>
               center_text(context, self.radius + 15.0, "W");
            when others =>
               center_text(context, self.radius + 15.0, Integer'Image(x*3));
         end case;
      end loop;
   end;
   --
   -- The compass actually has three pointers.  The large pointer gives the
   -- altitude mod 100 feet, the small pointer gives the altitude mod 1000 feet,
   -- and the triangle gives the altitude mod 10000 feet.
   --
   procedure draw_pointers(Self : access bbs_compass_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
   begin
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi));
      Cairo.Set_Line_Width(context, 2.0);
      Cairo.Get_Matrix(context, matrix'Access);
      --
      -- Draw triangle
      --
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, Glib.Gdouble(two_pi/1000.0));
      Cairo.Scale(context, Glib.Gdouble(float(self.size) / 300.0),
                  Glib.Gdouble(float(self.size) / 300.0));
      set_color(context, color_grey9);
      Cairo.Move_To(context, 5.0, 75.0);
      Cairo.Line_To(context, 0.0, 115.0);
      Cairo.Line_To(context, -5.0, 75.0);
      Cairo.Line_To(context, 5.0, 75.0);
      Cairo.Fill(context);
      --
      -- Draw airplane symbol
      --
      Cairo.Move_To(context, 0.0, 70.0);
      Cairo.Line_To(context, 5.0, 10.0);
      Cairo.Line_To(context, 55.0, 5.0);
      Cairo.Line_To(context, 55.0, -5.0);
      Cairo.Line_To(context, 5.0, -5.0);
      Cairo.Line_To(context, 5.0, -55.0);
      Cairo.Line_To(context, 30.0, -65.0);
      Cairo.Line_To(context, 30.0, -70.0);
      Cairo.Line_To(context, -30.0, -70.0);
      Cairo.Line_To(context, -30.0, -65.0);
      Cairo.Line_To(context, -5.0, -55.0);
      Cairo.Line_To(context, -5.0, -5.0);
      Cairo.Line_To(context, -55.0, -5.0);
      Cairo.Line_To(context, -55.0, 5.0);
      Cairo.Line_To(context, -5.0, 10.0);
      Cairo.Line_To(context, 0.0, 70.0);
      Cairo.Fill(context);
      --
      -- Draw other lines
      --
      Cairo.Set_Line_Width(context, 1.0);
      Cairo.Move_To(context, 0.0, -75.0);
      Cairo.Line_To(context, 0.0, -115.0);
      --
      Cairo.Move_To(context, 70.0, 0.0);
      Cairo.Line_To(context, 115.0, 0.0);
      --
      Cairo.Move_To(context, -70.0, 0.0);
      Cairo.Line_To(context, -115.0, 0.0);
      Cairo.Stroke(context);
   end;
   --
   -- This function draws a red X on the widget area.  It is used when data for
   -- the widget is failed.
   --
   procedure draw_failed(Self : access bbs_compass_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Line_Width(context, 5.0);
      set_color(context, color_white);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), Glib.Gdouble(Float(self.size)));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(Float(self.size)));
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), 0.0);
      Cairo.Stroke(context);
   end;

end;
