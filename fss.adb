
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with devicesFSS_V1; use devicesFSS_V1;

-- NO ACTIVAR ESTE PAQUETE MIENTRAS NO SE TENGA PROGRAMADA LA INTERRUPCION
-- Packages needed to generate button interrupts       
-- with Ada.Interrupts.Names;
-- with Button_Interrupt; use Button_Interrupt;

package body fss is

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    -----------------------------------------------------------------------
    ------------- declaration of protected objects 
    -----------------------------------------------------------------------

    -- Aqui se declaran los objetos protegidos para los datos compartidos  

    protected Pitch_Roll_Command is
      procedure Get_Joystick (J: out Joystick_Samples_Type);
    end Pitch_Roll_Command;
    
    protected body Pitch_Roll_Command is
      procedure Get_Joystick (J: out Joystick_Samples_Type) is
      begin
        Read_Joystick (J);
      end Get_Joystick;
    end Pitch_Roll_Command;

    protected Pitch_Roll is
      procedure Change_Aircraft_Pitch (P: in Pitch_Samples_Type);
      procedure Change_Aircraft_Roll (R: in Roll_Samples_Type);
    end Pitch_Roll;

    protected body Pitch_Roll is
      procedure Change_Aircraft_Pitch (P: in Pitch_Samples_Type) is
      begin
        Set_Aircraft_Pitch (P);
      end Change_Aircraft_Pitch;
      
      procedure Change_Aircraft_Roll (R: in Roll_Samples_Type) is
      begin
        Set_Aircraft_Roll (R);
      end Change_Aircraft_Roll;
    end Pitch_Roll;

    protected Current_Speed_Altitude is
      function Get_Speed return Speed_Samples_Type;
      function Get_Altitude return Speed_Samples_Type;
    end Current_Speed_Altitude;

    protected body Current_Speed_Altitude is
      function Get_Speed return Speed_Samples_Type is
      begin
        return Read_Speed;
      end;
      
      function Get_Altitude return Altitude_Samples_Type is
      begin
        return Read_Altitude;
      end;
    end Current_Speed_Altitude;

    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    -- Aqui se declaran las tareas que forman el STR

    task Task_Control_Cabeceo_Altitud is
        pragma Priority (14);
    end Task_Control_Cabeceo_Altitud;

    task Task_Control_Alabeo is
        pragma Priority (13);
    end Task_Control_Alabeo;

    task Task_Control_Velocidad is
        pragma Priority (15);
    end Task_Control_Velocidad;

    task Task_Deteccion_Obstaculos is
        pragma Priority (12);
    end Task_Deteccion_Obstaculos;

    task Task_Prueba_Sensores_Piloto is
        pragma Priority (10);
    end Task_Prueba_Sensores_Piloto;

    -----------------------------------------------------------------------
    ------------- body of tasks 
    -----------------------------------------------------------------------

    -- Aqui se escriben los cuerpos de las tareas 

   task body Task_Control_Cabeceo_Altitud is
        Next_Instance: Time;
        Interval: Time_Span := Milliseconds(200);

        Current_J: Joystick_Samples_Type;
        Current_A: Altitude_Samples_Type;
        Current_P: Pitch_Samples_Type;
        Target_Pitch: Pitch_Samples_Type := 0;
        Aircraft_Pitch: Pitch_Samples_Type; 
        Change_Pitch: Pitch_Increment_Type; 

        Max_Pitch: constant Pitch_Samples_Type := 30;
        Min_Pitch: constant Pitch_Samples_Type := -30;
        Low_Altitude: constant Altitude_Samples_Type := 2500;
        High_Altitude: constant Altitude_Samples_Type := 9500;
        Min_Altitude: constant Altitude_Samples_Type := 2000;
        Max_Altitude: constant Altitude_Samples_Type := 10000;
   begin
      loop
          Start_Activity ("Task_Control_Cabeceo_Altitud");  

          -- Lee Joystick del piloto, altitud, pitch de la aeronave
          Current_A := Read_Altitude;
          Pitch_Roll_Command.Get_Joystick (Current_J);
          Current_P := Read_Pitch;
          
          -- Establece Pitch deseado en la aeronave
          Target_Pitch := Pitch_Samples_Type (Current_J(x));

          -- Si pitch se encuentra entre +30/-30 grados el FSS lo refleja en la posicion de la nave
          if (Target_Pitch > Min_Pitch and Target_Pitch < Max_Pitch) then
            Set_Aircraft_Pitch (Target_Pitch);
          end if;

          -- Regula si altitud sobrepasa limite de altitud baja o alta
          if (Current_A < Min_Altitude or Current_A > Max_Altitude) then
            Set_Aircraft_Pitch (0);
          end if;

          -- Alerta mediante luces en caso de altitud alta o baja
          if (Current_A < Low_Altitude) then
            Light_2 (Off);
            Light_1 (On);
          elsif (Current_A > High_Altitude) then
            Light_1 (Off);
            Light_2 (On);
          end if;

          Finish_Activity ("Task_Control_Cabeceo_Altitud");
          -- Se realiza 5 veces por segundo
          delay until Next_Instance;
          Next_Instance := Next_Instance + Interval;
      end loop;
   end Task_Control_Cabeceo_Altitud;

   task body Task_Control_Alabeo is
        Next_Instance: Time;
        Interval: Time_Span := Milliseconds(200);

        Current_J: Joystick_Samples_Type := (0,0);
        Current_R: Roll_Samples_Type;
        Target_Roll: Roll_Samples_Type; 
        Min_Roll: constant Roll_Samples_Type := -45;
        Max_Roll: constant Roll_Samples_Type := 45;
        Low_Roll: constant Roll_Samples_Type := -35;
        High_Roll: constant Roll_Samples_Type := -35;
   begin
      loop
          Start_Activity ("Task_Control_Alabeo");  

          -- Lee Joystick del piloto y roll de la aeronave
          Pitch_Roll_Command.Get_Joystick (Current_J);
          Current_R := Read_Roll;
          
          -- Establece Roll deseado en la aeronave
          Target_Roll := Roll_Samples_Type (Current_J(y));

          -- Si roll se encuentra entre +45/-45 grados el FSS lo refleja en la posicion de la nave
          if (Target_Roll > Min_Roll and Target_Roll < Max_Roll) then
            Set_Aircraft_Roll (Target_Roll);
            Current_R := Target_Roll;
          end if;

          -- Mensaje en display en caso de roll alto o bajo
          if (Current_R < Low_Roll or Current_R > High_Roll) then
            Display_Roll (Current_R);
          end if;

          Finish_Activity ("Task_Control_Alabeo");
          -- Se realiza 5 veces por segundo
          delay until Next_Instance;
          Next_Instance := Next_Instance + Interval;
      end loop;
   end Task_Control_Alabeo;

   task body Task_Control_Velocidad is
        Next_Instance: Time;
        Interval: Time_Span := Milliseconds(300);

        Current_Pw: Power_Samples_Type := 0;
        Calculated_S: Speed_Samples_type := 0; 

        Current_J: Joystick_Samples_Type := (0,0);
        Target_Pitch: Pitch_Samples_Type := 0;
        Target_Roll: Roll_Samples_Type := 0; 
             
        Current_D: Distance_Samples_Type := 0;
        Current_L: Light_Samples_Type := 0;
        
        Pitch_Roll_Additional_Speed: constant Speed_Samples_Type := 200;
        Pitch_Additional_Speed: constant Speed_Samples_Type := 150;
        Roll_Additional_Speed: constant Speed_Samples_Type := 100;
        High_Speed: constant Speed_Samples_type := 1000;
        Low_Speed: constant Speed_Samples_Type := 300;
    begin
        Next_Instance := Big_Bang + Interval;
        loop
            Start_Activity ("Task_Control_Velocidad");        
                       
            -- Lee potencia del piloto y muestra al piloto
            Read_Power (Current_Pw); 
            Display_Pilot_Power (Current_Pw);
                          
            -- Transfiere la potencia/velocidad a la aeronave
            Calculated_S := Speed_Samples_type (float (Current_Pw) * 1.2); -- aplicar fórmula
            
            -- Lee Joystick del piloto
            Pitch_Roll_Command.Get_Joystick (Current_J);
            
            -- Establece Pitch y Roll deseado en la aeronave
            Target_Pitch := Pitch_Samples_Type (Current_J(x));
            Target_Roll := Roll_Samples_Type (Current_J(y));  

            -- Velocidad adicional en diferentes maniobras
            if (Target_Pitch /= 0 and Target_Roll /= 0) then
               Calculated_S := Calculated_S + Pitch_Roll_Additional_Speed;
            elsif (Target_Pitch /= 0) then
               Calculated_S := Calculated_S + Pitch_Additional_Speed;
            elsif (Target_Roll /= 0) then
               Calculated_S := Calculated_S + Roll_Additional_Speed;
            end if;

            -- Actualizar velocidad
            Set_Speed (Calculated_S);

            -- Control alta velocidad y luces
            if Calculated_S > High_Speed then
               Set_Speed (High_Speed);
               Light_1 (Off);
               Light_2 (On);
            elsif Calculated_S < Low_Speed then
               Set_Speed (Low_Speed);
               Light_1 (Off);
               Light_2 (On);
            else
              Set_Speed (Calculated_S);
              Light_2 (Off);
              Light_1 (On);
            end if;
            
            Finish_Activity ("Task_Control_Velocidad"); 
            delay until Next_Instance;
            Next_Instance := Next_Instance + Interval;
        end loop;
    end Task_Control_Velocidad;

    task body Task_Deteccion_Obstaculos is
        Next_Instance: Time;
        Interval: Time_Span := Milliseconds(250);

        Current_D: Distance_Samples_Type;
        Current_L: Light_Samples_Type;
        Current_S: Speed_Samples_Type;
        Current_P: PilotPresence_Samples_Type;

        Time_Collision: Duration;

        Light_Threshold: constant Light_Samples_Type := 500;
        Max_D: constant Distance_Samples_Type := 5000;
        Alarm_Time_Threshold_General: constant Duration := 10.0;
        Alarm_Time_Threshold_Bad_Conditions: constant Duration := 15.0;
        Alarm_Time_Threshold: Duration;
        Time_Collision_Threshold_General: constant Duration := 5.0;
        Time_Collision_Threshold_Bad_Conditions: constant Duration := 10.0;
        Time_Collision_Threshold: Duration;

        Emergency_Roll: constant Roll_Samples_Type := 45;
        Emergency_Roll_Duration: constant Time_Span:= Milliseconds(3000);

    begin
        Next_Instance := Big_Bang + Interval;
        loop
            Start_Activity ("Task_Deteccion_Obstaculos");
            
            -- Detectar variables externas
            Read_Distance(Current_D);
            Read_Light_Intensity(Current_L);
            Current_S := Read_Speed;
            Current_P := Read_PilotPresence;
            
            -- Calcular tiempo de colision
            Time_Collision := Duration (Float(Current_D) / Float(Current_S));

            -- Modificar thresholds para diferentes casos
            if (Current_L < Light_Threshold or Current_P = 0) then
              Alarm_Time_Threshold := Alarm_Time_Threshold_Bad_Conditions;
              Time_Collision_Threshold := Time_Collision_Threshold_Bad_Conditions;
            else
              Alarm_Time_Threshold := Alarm_Time_Threshold_General;
              Time_Collision_Threshold := Time_Collision_Threshold_General;
            end if;

            -- Maniobra de desvio automatico
            if (Time_Collision < Time_Collision_Threshold) then
              -- 45 grados roll a la derecha durante 3 segundos
              Pitch_Roll.Change_Aircraft_Roll (Emergency_Roll);
              delay until (Clock + Emergency_Roll_Duration);
              -- estabilizar roll
              Pitch_Roll.Change_Aircraft_Roll (0);
            end if;

            -- Indicar distancia de obstaculo si existe
            if (Current_D <= Max_D) then
              Display_Distance (Current_D);
            end if;

            -- Aviso a piloto 
            if (Time_Collision < Alarm_Time_Threshold) then
              Alarm (4);
            end if;

            Finish_Activity ("Task_Deteccion_Obstaculos");
            delay until Next_Instance;
            Next_Instance := Next_Instance + Interval;
        end loop;
    end Task_Deteccion_Obstaculos;

    task body Task_Prueba_Sensores_Piloto is
        Next_Instance: Time;
        Interval: Time_Span := Milliseconds(300);

        Current_Pp: PilotPresence_Samples_Type := 1;
        Current_Pb: PilotButton_Samples_Type := 0;
    begin
        Next_Instance := Big_Bang + Interval;
        loop
            Start_Activity ("Task_Prueba_Sensores_Piloto");                
            -- Prueba presencia piloto
            Current_Pp := Read_PilotPresence;
            if (Current_Pp = 0) then 
                Alarm (1); 
            end if;   
            Display_Pilot_Presence (Current_Pp);
                     
            -- Prueba botón para selección de modo 
            Current_Pb := Read_PilotButton;            
            Display_Pilot_Button (Current_Pb); 
            
            Finish_Activity ("Task_Prueba_Sensores_Piloto");  
            delay until Next_Instance;
            Next_Instance := Next_Instance + Interval;
        end loop;
    end Task_Prueba_Sensores_Piloto;


    ----------------------------------------------------------------------
    ------------- procedimientos para probar los dispositivos 
    ------------- SE DEBERÁN QUITAR PARA EL PROYECTO
    ----------------------------------------------------------------------

    --quitados

begin
   Start_Activity ("Programa Principal");
   -- Tasks start automatically
   Finish_Activity ("Programa Principal");
end fss;


