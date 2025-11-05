
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with devicesFSS_V1; use devicesFSS_V1;

-- Integrantes del grupo:
-- Leonardo Chen
-- Pablo Gil
-- Mario Lorenzo
-- Zixin Zheng

-- Tareas implementadas:
-- Control de cabeceo y altitud (Task_Control_Cabeceo_Altitud)
-- Control de alabeo (Task_Control_Alabeo)
-- Control de velocidad (Task_Control_Velocidad)

-- Objetos protegidos implementados:
-- Sincronizacion de datos de joystick (Pitch_Roll_Command)

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

    -- Accedido por: Task_Control_Cabeceo_Altitud (prio 14), Task_Control_Alabeo (prio 13),
    --               Task_Control_Velocidad (prio 15).
    -- Techo (ceiling) = 15 (máxima prioridad de los llamadores).
    -- Fuente del ceiling: en el Protocolo de Techo de Prioridad de Ada, el ceiling
    -- del objeto protegido debe ser la máxima prioridad de las tareas que van a
    -- llamar a sus operaciones; por eso se toma max(14,13,15) = 15.
    protected Pitch_Roll_Command is
      pragma Priority (15);
      procedure Get_Joystick (J: out Joystick_Samples_Type);
    end Pitch_Roll_Command;
    
    protected body Pitch_Roll_Command is
      procedure Get_Joystick (J: out Joystick_Samples_Type) is
      begin
        Read_Joystick (J);
      end Get_Joystick;
    end Pitch_Roll_Command;

    -- Accedido por: Task_Control_Cabeceo_Altitud (prio 14),
    --               Task_Control_Velocidad (prio 15)
    -- Fuente del ceiling: ceiling = max(14,15) = 15.
    -- Techo (ceiling) = 15.
    protected Pitch is
      pragma Priority (15);
      procedure Change_Aircraft_Pitch (P: in Pitch_Samples_Type);
    end Pitch;

    protected body Pitch is
      procedure Change_Aircraft_Pitch (P: in Pitch_Samples_Type) is
      begin
        Set_Aircraft_Pitch (P);
      end Change_Aircraft_Pitch;
    end Pitch;

    -- Accedido por: Task_Control_Alabeo (prio 13),
    --               Task_Deteccion_Obstaculos (prio 20).
    -- Fuente del ceiling: ceiling = max(13,20) = 20.
    -- Techo (ceiling) = 20.
    protected Roll is
      pragma Priority (20);
      procedure Change_Aircraft_Roll (R: in Roll_Samples_Type);
    end Roll;

    protected body Roll is
      procedure Change_Aircraft_Roll (R: in Roll_Samples_Type) is
      begin
        Set_Aircraft_Roll (R);
      end Change_Aircraft_Roll;
    end Roll;

    -- Accedido por: Task_Control_Velocidad (prio 15),
    --               Task_Deteccion_Obstaculos (prio 20).
    -- Techo (ceiling) = 20.
    -- Fuente del ceiling: ceiling = max(15,20) = 20.
    protected Current_Speed is
      pragma Priority (20);
      function Get_Speed return Speed_Samples_Type;
    end Current_Speed;

    protected body Current_Speed is
      function Get_Speed return Speed_Samples_Type is
      begin
        return Read_Speed;
      end;
    end Current_Speed;

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
        pragma Priority (20);
    end Task_Deteccion_Obstaculos;

    -----------------------------------------------------------------------
    ------------- body of tasks 
    -----------------------------------------------------------------------

    -- Aqui se escriben los cuerpos de las tareas 

   task body Task_Control_Cabeceo_Altitud is
        Next_Instance: Time;
        Interval: constant Time_Span := Milliseconds(200);

        Current_J: Joystick_Samples_Type;
        Current_A: Altitude_Samples_Type;
        Current_P: Pitch_Samples_Type;

        Target_Pitch: Pitch_Samples_Type := 0;

        Max_Pitch: constant Pitch_Samples_Type := 30;
        Min_Pitch: constant Pitch_Samples_Type := -30;
        Low_Altitude: constant Altitude_Samples_Type := 2500;
        High_Altitude: constant Altitude_Samples_Type := 9500;
        Min_Altitude: constant Altitude_Samples_Type := 2000;
        Max_Altitude: constant Altitude_Samples_Type := 10000;
   begin
      loop
          Start_Activity ("Task_Control_Cabeceo_Altitud");  

          -- Lee Joystick del piloto y altitud de la aeronave
          Current_A := Read_Altitude;
          Pitch_Roll_Command.Get_Joystick (Current_J);
          
          -- Establece Pitch deseado en la aeronave
          Target_Pitch := Pitch_Samples_Type (Current_J(x));

          -- Si pitch deseado se encuentra entre +30/-30 grados el FSS lo refleja en la posicion de la nave
          if (Target_Pitch > Min_Pitch and Target_Pitch < Max_Pitch) then
            Pitch.Change_Aircraft_Pitch (Target_Pitch);
          end if;

          -- Regula si altitud sobrepasa limite de altitud baja o alta
          if (Current_A < Min_Altitude or Current_A > Max_Altitude) then
            Pitch.Change_Aircraft_Pitch (0);
          end if;

          -- Alerta mediante luces en caso de altitud alta o baja
          if (Current_A < Low_Altitude) then
            Light_2 (Off);
            Light_1 (On);
          elsif (Current_A > High_Altitude) then
            Light_1 (Off);
            Light_2 (On);
          end if;

          -- Display de pitch y altitud
          Current_P := Read_Pitch;
          Display_Pitch (Current_P);
          Display_Altitude (Current_A);

          Finish_Activity ("Task_Control_Cabeceo_Altitud");
          -- Se realiza 5 veces por segundo
          delay until Next_Instance;
          Next_Instance := Next_Instance + Interval;
      end loop;
   end Task_Control_Cabeceo_Altitud;

   task body Task_Control_Alabeo is
        Next_Instance: Time;
        Interval: constant Time_Span := Milliseconds(200);

        Current_J: Joystick_Samples_Type := (0,0);
        Current_R: Roll_Samples_Type;
        
        Target_Roll: Roll_Samples_Type; 

        Min_Roll: constant Roll_Samples_Type := -45;
        Max_Roll: constant Roll_Samples_Type := 45;
        Low_Roll: constant Roll_Samples_Type := -35;
        High_Roll: constant Roll_Samples_Type := 35;
        Warning_Message: constant String := "WARNING: HIGH ROLL ANGLE!";
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
            Roll.Change_Aircraft_Roll (Target_Roll);
            Current_R := Target_Roll;
          end if;

          -- Mensaje en display en caso de roll alto o bajo
          if (Current_R < Low_Roll or Current_R > High_Roll) then
            Display_Message (Warning_Message);
          end if;

          -- Display de roll
          Display_Roll (Current_R);

          Finish_Activity ("Task_Control_Alabeo");
          -- Se realiza 5 veces por segundo
          delay until Next_Instance;
          Next_Instance := Next_Instance + Interval;
      end loop;
   end Task_Control_Alabeo;

   task body Task_Control_Velocidad is
        Next_Instance: Time;
        Interval: constant Time_Span := Milliseconds(300);

        Current_Pw: Power_Samples_Type := 0;
        Current_J: Joystick_Samples_Type := (0,0);
        Current_S: Speed_Samples_Type := 0;

        Calculated_S: Speed_Samples_type := 0; 
        Input_Speed: Speed_Samples_Type := 0;
        Target_Pitch: Pitch_Samples_Type := 0;
        Target_Roll: Roll_Samples_Type := 0; 
        
        Pitch_Roll_Additional_Speed: constant Speed_Samples_Type := 200;
        Pitch_Additional_Speed: constant Speed_Samples_Type := 150;
        Roll_Additional_Speed: constant Speed_Samples_Type := 100;
        Max_Speed: constant Speed_Samples_type := 1000;
        Min_Speed: constant Speed_Samples_Type := 300;
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

            -- Control alta velocidad y luces
            if Calculated_S > Max_Speed then
               Input_Speed := Max_Speed;
               Light_1 (Off);
               Light_2 (On);
            elsif Calculated_S < Min_Speed then
               Input_Speed := Min_Speed;
               Light_1 (Off);
               Light_2 (On);
            else
              Input_Speed := Calculated_S;
              Light_2 (Off);
              Light_1 (On);
            end if;
            Set_Speed (Input_Speed);

            -- Display de velocidad
            Current_S := Current_Speed.Get_Speed;
            Display_Speed(Current_S);

            Finish_Activity ("Task_Control_Velocidad");
            delay until Next_Instance;
            Next_Instance := Next_Instance + Interval;
        end loop;
    end Task_Control_Velocidad;

    task body Task_Deteccion_Obstaculos is
        Next_Instance: Time;
        Interval: constant Time_Span := Milliseconds(250);

        Current_D: Distance_Samples_Type;
        Current_L: Light_Samples_Type;
        Current_S: Speed_Samples_Type;
        Current_P: PilotPresence_Samples_Type;

        Time_Collision: Duration;
        Alarm_Time_Threshold: Duration;
        Time_Collision_Threshold: Duration;

        Light_Threshold: constant Light_Samples_Type := 500;
        Max_D: constant Distance_Samples_Type := 5000;
        Alarm_Time_Threshold_General: constant Duration := 10.0;
        Alarm_Time_Threshold_Bad_Conditions: constant Duration := 15.0;
        Time_Collision_Threshold_General: constant Duration := 5.0;
        Time_Collision_Threshold_Bad_Conditions: constant Duration := 10.0;
        Emergency_Roll: constant Roll_Samples_Type := 45;
        Emergency_Roll_Duration: constant Time_Span:= Milliseconds(3000);

    begin
        Next_Instance := Big_Bang + Interval;
        loop
            Start_Activity ("Task_Deteccion_Obstaculos");
            
            -- Detectar variables externas
            Read_Distance(Current_D);
            Read_Light_Intensity(Current_L);
            Current_S := Current_Speed.Get_Speed;
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
              -- TODO Plazo maximo de 80 milisegundos para ejecutar la maniobra
              Roll.Change_Aircraft_Roll (Emergency_Roll);
              delay until (Clock + Emergency_Roll_Duration);
              -- Estabilizar roll
              Roll.Change_Aircraft_Roll (0);
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


