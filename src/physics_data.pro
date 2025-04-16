FUNCTION electron_wavelength, voltage
  ;; voltage in V
return, 1.22639/Sqrt(voltage + 0.00000097845*voltage*voltage);
END
