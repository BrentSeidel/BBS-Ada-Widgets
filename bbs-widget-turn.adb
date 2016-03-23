package body bbs.widget.turn is

   function Get_Type return Glib.GType is
   begin
       Glib.Object.Initialize_Class_Record
         (Ancestor     => Gtk.Drawing_Area.Get_Type,
          Class_Record => Klass,
          Type_Name    => "bbs_turn",
          Signals      => Glib.Object.No_Signals,
          Parameters   => Glib.Object.Null_Parameter_Types);
      return Klass.The_Type;
   end;
   --
   procedure gtk_new(self : in out bbs_turn) is
   begin
      self := new bbs_turn_record;
      initialize(self);
   end;
   --
   procedure initialize(self : not null access bbs_turn_record'class) is
   begin
      Glib.Object.G_New(Object => self,
                        Typ    => Get_Type);
      Gtk.Drawing_Area.Initialize(self);
      self.On_Draw(draw_turn'access, True);
   end;
   --
   -- Attach the window and request the size as well as setting reasonable
   -- defaults for most values.
   --
   procedure setup(self : not null access bbs_turn_record'class;
                  width : Integer; parent : Gdk.Gdk_Window) is
   begin
      self.value := 0.0;
      self.pointer := 0.0;
      self.radius := Float(width - 50)/2.0;
      self.size := width;
      self.slip := 0.0;
      self.slew := false;
      self.failed := False;
      Glib.Properties.Set_Property(self, Gtk.Widget.Height_Request_Property, Glib.Gint(self.size));
      Glib.Properties.Set_Property(self, Gtk.Widget.Width_Request_Property, Glib.Gint(self.size));
      self.Set_Window(parent);
      self.Set_Has_Window(True);
   end;
   --
   procedure set_value(self : in out bbs_turn_record'Class; value : Float) is
   begin
      if (value > 6.0) then
         self.value := 6.0;
      else
         if (value < -6.0) then
            self.value := -6.0;
         else
            self.value := value;
         end if;
      end if;
      if (not self.slew) then
         self.pointer := self.value;
         self.Queue_Draw;
      end if;
   end;

   procedure set_failed(self : in out bbs_turn_record'Class; value : Boolean) is
   begin
      self.failed := value;
      self.Queue_Draw;
   end;
   --
   procedure set_slip(self : in out bbs_turn_record'Class; value : Float) is
   begin
      if (value > 1.0) then
         self.slip := 1.0;
      else
         if (value < -1.0) then
            self.slip := -1.0;
         else
            self.slip := value;
         end if;
      end if;
      self.Queue_Draw;
   end;
   --
   procedure set_slew(self : in out bbs_turn_record'Class; value : Boolean; rate : Float) is
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
      me : constant bbs_turn := bbs_turn(self);
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

   function draw_turn(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean is
      me : constant bbs_turn := bbs_turn(self);
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
         Cairo.Set_Matrix(context, matrix'Access);
         draw_pointers(me, context);
      end if;
      return True;
   end;

   procedure draw_background(Self : access bbs_turn_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
      font_mat : aliased Cairo.Cairo_Matrix;
      ex : aliased Cairo.Cairo_Text_Extents;
      text1 : constant String := "TURN COORDINATOR";
      text2 : constant String := "NO PITCH";
      text3 : constant String := "INFORMATION";
      text4 : constant String := "2 MIN";
   begin
      Cairo.Set_Source_Rgb(context, 0.0, 0.0, 0.0);
      Cairo.Paint(context);
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi));
      Cairo.Get_Matrix(context, matrix'Access);
      Cairo.Get_Font_Matrix(context, font_mat'Access);
      --
      -- Draw circle
      --
      Cairo.Set_Source_Rgb(context, 0.1, 0.1, 0.1);
      Cairo.Arc(context, 0.0, 0.0, 103.0, 0.0, Glib.Gdouble(two_pi));
      Cairo.Fill(context);
      --
      -- First draw the major ticks
      --
      Cairo.Set_Source_Rgb(context, 1.0, 1.0, 1.0);
      Cairo.Set_Line_Width(context, 4.0);
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Move_To(context, 105.0, 0.0);
      Cairo.Line_To(context, 125.0, 0.0);
      Cairo.Move_To(context, -105.0, 0.0);
      Cairo.Line_To(context, -125.0, 0.0);
      Cairo.Rotate(context, Glib.Gdouble(15.0*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, -105.0, 0.0);
      Cairo.Line_To(context, -125.0, 0.0);
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, -Glib.Gdouble(15.0*Ada.Numerics.Pi/180.0));
      Cairo.Move_To(context, 105.0, 0.0);
      Cairo.Line_To(context, 125.0, 0.0);
      Cairo.Stroke(context);
      --
      -- Other labels
      --
      Cairo.Set_Source_Rgb(context, 0.9, 0.9, 0.9);
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Set_Font_Matrix(context, font_mat'Access);
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi));
      Cairo.Set_Font_Size(context, 9.0);
      Cairo.Text_Extents(context, Interfaces.C.Strings.New_String(text1), ex'Access);
      Cairo.Move_To(context, -ex.Width/2.0, 15.0);
      Cairo.Show_Text(context, text1);
      Cairo.Text_Extents(context, Interfaces.C.Strings.New_String(text2), ex'Access);
      Cairo.Move_To(context, -ex.Width/2.0, 65.0);
      Cairo.Show_Text(context, text2);
      Cairo.Text_Extents(context, Interfaces.C.Strings.New_String(text3), ex'Access);
      Cairo.Move_To(context, -ex.Width/2.0, 75.0);
      Cairo.Show_Text(context, text3);
      --
      Cairo.Set_Font_Size(context, 12.0);
      Cairo.Text_Extents(context, Interfaces.C.Strings.New_String(text4), ex'Access);
      Cairo.Move_To(context, -ex.Width/2.0, 50.0);
      Cairo.Show_Text(context, text4);
      Cairo.Move_To(context, -110.0, 50.0);
      Cairo.Show_Text(context, "L");
      Cairo.Move_To(context, 105.0, 50.0);
      Cairo.Show_Text(context, "R");
      --
      -- Draw line for slip indicator
      --
      Cairo.Set_Source_Rgb(context, 0.7, 0.7, 0.7);
      Cairo.Set_Line_Cap(context, Cairo.Cairo_Line_Cap_Round);
      Cairo.Set_Line_Width(context, 12.0);
      Cairo.Move_To(context, -30.0, 30.0);
      Cairo.Line_To(context, 30.0, 30.0);
      Cairo.Stroke(context);
      --
   end;
   --
   procedure draw_pointers(Self : access bbs_turn_record'Class; context : Cairo.Cairo_Context) is
      matrix : aliased Cairo.Cairo_Matrix;
   begin
      Cairo.Rotate(context, Glib.Gdouble(Ada.Numerics.Pi));
      Cairo.Get_Matrix(context, matrix'Access);
      --
      -- Draw slip ball and cage
      --
      Cairo.Set_Source_Rgb(context, 0.1, 0.1, 0.1);
      Cairo.Set_Line_Width(context, 2.0);
      Cairo.Move_To(context, -7.0, -24.0);
      Cairo.Line_To(context, -7.0, -36.0);
      Cairo.Move_To(context, 7.0, -24.0);
      Cairo.Line_To(context, 7.0, -36.0);
      Cairo.Stroke(context);
      --
      Cairo.Set_Source_Rgb(context, 0.9, 0.9, 0.9);
      Cairo.Arc(context, -Glib.Gdouble(self.slip*30.0), -30.0, 6.0, 0.0, Glib.Gdouble(two_pi));
      Cairo.Fill(context);
      --
      -- Draw airplane symbol
      --
      Cairo.Set_Line_Width(context, 4.0);
      Cairo.Set_Matrix(context, matrix'Access);
      Cairo.Rotate(context, -Glib.Gdouble(self.pointer*5.0*Ada.Numerics.Pi/180.0));
      Cairo.Set_Source_Rgb(context, 0.9, 0.9, 0.9);
      Cairo.Move_To(context, -100.0, 0.0);
      Cairo.Line_To(context, 100.0, 0.0);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Line_To(context, 0.0, 20.0);
      Cairo.Move_To(context, -20.0, 10.0);
      Cairo.Line_To(context, 20.0, 10.0);
      Cairo.Stroke(context);
   end;
   --
   -- This function draws a red X on the widget area.  It is used when data for
   -- the widget is failed.
   --
   procedure draw_failed(Self : access bbs_turn_record'Class; context : Cairo.Cairo_Context) is
   begin
      Cairo.Set_Line_Width(context, 5.0);
      Cairo.Set_Source_Rgb(context, 1.0, 0.0, 0.0);
      Cairo.Move_To(context, 0.0, 0.0);
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), Glib.Gdouble(Float(self.size)));
      Cairo.Move_To(context, 0.0, Glib.Gdouble(Float(self.size)));
      Cairo.Line_To(context, Glib.Gdouble(Float(self.size)), 0.0);
      Cairo.Stroke(context);
   end;

end;
