package body bbs.widget.dial is

   function Get_Type return Glib.GType is
   begin
       Glib.Object.Initialize_Class_Record
         (Ancestor     => Gtk.Drawing_Area.Get_Type,
          Class_Record => Klass,
          Type_Name    => "bbs_dial",
          Signals      => Glib.Object.No_Signals,
          Parameters   => Glib.Object.Null_Parameter_Types);
      return Klass.The_Type;
   end;

   procedure gtk_new(self : in out bbs_dial) is
   begin
      self := new bbs_dial_record;
      initialize(self);
   end;

   procedure initialize(self : not null access bbs_dial_record'class) is
   begin
      Glib.Object.G_New(Object => self,
                        Typ    => Get_Type);
      Gtk.Drawing_Area.Initialize(self);
      self.On_Draw(draw_dial'access, True);
   end;
   --
   -- Attach the window and request the size as well as setting reasonable
   -- defaults for most values.
   --
   procedure setup(self : not null access bbs_dial_record'class; minimum : float; maximum : float;
                  radius : Float; parent : Gdk.Gdk_Window) is
   begin
      self.min := minimum;
      self.max := maximum;
      self.value := minimum;
      self.pointer := minimum;
      self.major := 10;
      self.minor := 0;
      self.radius := radius;
      self.size := Integer(self.radius*2.0 + 50.0);
      self.slew := false;
      self.failed := False;
      self.park := False;
      self.arc := False;
      self.arc_start := 0.0;
      self.arc_end := two_pi;
      self.fill := False;
      self.low_red := minimum;
      self.low_yellow := minimum;
      self.high_yellow := maximum;
      self.high_red := maximum;
      self.low_red_present := False;
      self.low_yellow_present := False;
      self.high_yellow_present := False;
      self.high_red_present := False;
      Glib.Properties.Set_Property(self, Gtk.Widget.Height_Request_Property, Glib.Gint(self.size));
      Glib.Properties.Set_Property(self, Gtk.Widget.Width_Request_Property, Glib.Gint(self.size));
      self.Set_Window(parent);
      self.Set_Has_Window(True);
   end;

   procedure set_value(self : in out bbs_dial_record'Class; value : Float) is
   begin
      self.value := value;
      if (self.park) then
         if (value < self.min) then
            self.value := self.min;
         end if;
         if (value > self.max) then
            self.value := self.max;
         end if;
      end if;
      if (not self.slew) then
         self.pointer := self.value;
         self.Queue_Draw;
      end if;
   end;

   procedure set_low_red(self : in out bbs_dial_record'Class; valid : Boolean; value : Float) is
   begin
      self.low_red := value;
      self.low_red_present := valid;
      self.Queue_Draw;
   end;

   procedure set_low_yellow(self : in out bbs_dial_record'Class; valid : Boolean; value : Float) is
   begin
      self.low_yellow := value;
      self.low_yellow_present := valid;
      self.Queue_Draw;
   end;

   procedure set_high_yellow(self : in out bbs_dial_record'Class; valid : Boolean; value : Float) is
   begin
      self.high_yellow := value;
      self.high_yellow_present := valid;
      self.Queue_Draw;
   end;

   procedure set_high_red(self : in out bbs_dial_record'Class; valid : Boolean; value : Float) is
   begin
      self.high_red := value;
      self.high_red_present := valid;
      self.Queue_Draw;
   end;

   procedure set_failed(self : in out bbs_dial_record'Class; value : Boolean) is
   begin
      self.failed := value;
      self.Queue_Draw;
   end;

   procedure set_fill(self : in out bbs_dial_record'Class; value : Boolean) is
   begin
      self.fill := value;
      self.Queue_Draw;
   end;

   procedure set_arc(self : in out bbs_dial_record'Class; value : Boolean) is
   begin
      self.arc := value;
      self.arc_start := 0.5;
      self.arc_end := two_pi - 0.5;
      self.Queue_Draw;
   end;

   procedure set_arc(self : in out bbs_dial_record'Class; value : Boolean; start : Float; finish : Float) is
   begin
      self.arc := value;
      self.arc_start := start;
      self.arc_end := finish;
      self.Queue_Draw;
   end;

   procedure set_ticks(self : in out bbs_dial_record'Class; major : Integer; minor : Integer) is
   begin
      self.major := major;
      self.minor := minor;
      self.Queue_Draw;
   end;

   procedure set_park(self : in out bbs_dial_record'Class; value : Boolean) is
   begin
      self.park := value;
      if (self.park) then
         if (self.slew) then
            if (self.value < self.min) then
               self.value := self.min;
            end if;
            if (self.value > self.max) then
               self.value := self.max;
            end if;
         else
            if (self.value < self.min) then
               self.value := self.min;
               self.pointer := self.min;
            end if;
            if (self.value > self.max) then
               self.value := self.max;
               self.pointer := self.min;
            end if;
            self.Queue_Draw;
         end if;
      end if;
   end;

   procedure set_slew(self : in out bbs_dial_record'Class; value : Boolean; rate : Float) is
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
      me : constant bbs_dial := bbs_dial(self);
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

   function draw_dial(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean is
      me : constant bbs_dial := bbs_dial(self);
      matrix : aliased Cairo.Cairo_Matrix;
   begin
      if (me.failed) then
         draw_failed(me, context);
      else
         Cairo.Translate(context, Glib.Gdouble(Float(me.size)/2.0),
                         Glib.Gdouble(Float(me.size)/2.0));
         Cairo.Get_Matrix(context, matrix'Access);
         me.draw_dial_arc(context);
         me.draw_ticks(context);
         Cairo.Set_Matrix(context, matrix'Access);
         if (me.fill) then
            draw_filled(me, context);
         else
            draw_pointer(me, context);
         end if;
      end if;
      return True;
   end;

   procedure draw_dial_arc(Self : access bbs_dial_record'Class; context : Cairo.Cairo_Context) is
      start : Glib.Gdouble;
      stop : Glib.Gdouble;
      green_start : Glib.Gdouble;
      green_stop : Glib.Gdouble;
   begin
      start := Glib.Gdouble(self.arc_start + Ada.Numerics.Pi/2.0);
      Cairo.Set_Line_Width(context, 4.0);
      if (self.low_red_present) then
         set_color(context, color_red);
         stop := Glib.Gdouble(self.compute_angle(self.low_red - self.min));
         Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius), start, stop);
         Cairo.Stroke(context);
         start := stop;
      end if;
      if (self.low_yellow_present) then
         set_color(context, color_yellow);
         stop := Glib.Gdouble(self.compute_angle(self.low_yellow - self.min));
         Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius), start, stop);
         Cairo.Stroke(context);
         start := stop;
      end if;
      green_start := start;
      stop := Glib.Gdouble(self.arc_end + Ada.Numerics.Pi/2.0);
      if (self.high_red_present) then
         set_color(context, color_red);
         start := Glib.Gdouble(self.compute_angle(self.high_red - self.min));
         Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius), start, stop);
         Cairo.Stroke(context);
         stop := start;
      end if;
      if (self.high_yellow_present) then
         set_color(context, color_yellow);
         start := Glib.Gdouble(self.compute_angle(self.high_yellow - self.min));
         Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius), start, stop);
         Cairo.Stroke(context);
         stop := start;
      end if;
      green_stop := stop;
      set_color(context, color_green5);
      Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius), green_start, green_stop);
      Cairo.Stroke(context);
   end;

   procedure draw_ticks(Self : access bbs_dial_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
      ticks : Integer;
   begin
      set_color(context, color_black);
      Cairo.Get_Matrix(context, matrix'Access);
      --
      -- First draw the major ticks
      --
      if (self.arc) then
         ticks := self.major;
      else
         ticks := self.major - 1;
      end if;
      if (ticks > 0) then
         Cairo.Set_Line_Width(context, 2.0);
         for x in 0 .. ticks loop
            Cairo.Set_Matrix(context, matrix'Access);
            if (self.arc) then
               Cairo.Rotate(context, Glib.Gdouble(self.arc_start + float(x)*(self.arc_end - self.arc_start)/float(self.major)));
            else
               Cairo.Rotate(context, Glib.Gdouble(float(x)*two_pi/float(self.major)));
            end if;
            Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 10.0));
            Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius + 5.0));
            center_text(context, self.radius + 15.0,
                        Integer'Image(integer(self.min + float(x)*(self.max - self.min)/float(self.major))));
         end loop;
         Cairo.Stroke(context);
      end if;
      --
      -- Then draw the minor ticks
      --
      if (self.arc) then
         ticks := self.minor;
      else
         ticks := self.minor - 1;
      end if;
      if (ticks > 0) then
         Cairo.Set_Line_Width(context, 1.0);
         for x in 0 .. ticks loop
            Cairo.Set_Matrix(context, matrix'Access);
            if (self.arc) then
               Cairo.Rotate(context, Glib.Gdouble(self.arc_start + float(x)*(self.arc_end - self.arc_start)/float(self.minor)));
            else
               Cairo.Rotate(context, Glib.Gdouble(float(x)*two_pi/float(self.minor)));
            end if;
            Cairo.Move_To(context, 0.0, Glib.Gdouble(self.radius - 5.0));
            Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius));
         end loop;
         Cairo.Stroke(context);
      end if;
   end;

   procedure draw_pointer(Self : access bbs_dial_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Line_Width(context, 2.0);
      if (self.arc) then
         Cairo.Rotate(context, Glib.Gdouble(self.arc_start + (self.pointer - self.min)*(self.arc_end - self.arc_start)/(self.max - self.min)));
      else
         Cairo.Rotate(context, Glib.Gdouble((self.pointer - self.min)*two_pi/(self.max - self.min)));
      end if;
      set_color(context, color_black);
      Cairo.Move_To(context, 0.0, -10.0);
      Cairo.Line_To(context, 5.0, 0.0);
      Cairo.Line_To(context, 0.0, Glib.Gdouble(self.radius - 10.0));
      Cairo.Line_To(context, -5.0, 0.0);
      Cairo.Line_To(context, 0.0, -10.0);
      Cairo.Stroke(context);
   end;

   procedure draw_filled(Self : access bbs_dial_record'Class; context : Cairo.Cairo_Context) is
      position : constant Float := self.pointer - self.min;
   begin
      set_color(context, color_green);
      if ((self.low_red_present) and (position < (self.low_red - self.min))) then
         set_color(context, color_red);
      else
         if ((self.low_yellow_present) and (position < (self.low_yellow-self.min))) then
            set_color(context, color_yellow);
         end if;
      end if;
      if ((self.high_red_present) and (position > (self.high_red - self.min))) then
         set_color(context, color_red);
      else
         if ((self.high_yellow_present) and (position > (self.high_yellow - self.min))) then
            set_color(context, color_yellow);
         end if;
      end if;
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi/2.0));
      Cairo.Move_To(context, 0.0, 0.0);
      if (self.arc) then
         Cairo.Rotate(context, Glib.Gdouble(self.arc_start));
         Cairo.Line_To(context, Glib.Gdouble(self.radius), 0.0);
         Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius), 0.0,
                   Glib.Gdouble(position*(self.arc_end - self.arc_start)/(self.max - self.min)));
      else
         Cairo.Line_To(context, Glib.Gdouble(self.radius), 0.0);
         Cairo.Arc(context, 0.0, 0.0, Glib.Gdouble(self.radius), 0.0, Glib.Gdouble(position*two_pi/(self.max - self.min)));
      end if;
      Cairo.Line_To(context, 0.0, 0.0);
      Cairo.Fill(context);
   end;
   --
   -- This function draws a red X on the widget area.  It is used when data for
   -- the widget is failed.
   --
   procedure draw_failed(Self : access bbs_dial_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Line_Width(context, 5.0);
      set_color(context, color_fail);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), Glib.Gdouble(Float(self.size)));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(Float(self.size)));
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), 0.0);
      Cairo.Stroke(context);
   end;

   function compute_angle(Self : access bbs_dial_record'Class; value : Float) return Float is
   begin
      if (self.arc) then
         return Ada.Numerics.Pi/2.0 + self.arc_start + value*(self.arc_end - self.arc_start)/(self.max - self.min);
      else
         return Ada.Numerics.Pi/2.0 + value*two_pi/(self.max - self.min);
      end if;
   end;

end;
