unit VCL.Lazy.Types;

interface

type
  TLZOSVersion = (osUnknown, osWin95, osWin98, osWin98SE, osWinNT, osWinME,
    osWin2000, osWinXP, osWinVista, osWinSeven, osWin8, osWin81,
    osWin10, osWin11);
  TLZOSStartupState = (ssUnknown, ssNormal, ssSafeMode, ssSafeModeNetwork);
  TLZOSArchitecture = (oaUnkown, oaX86, oaX64);
  TLZOSElevationLevel = (elUnknown, elDefault, elFull, elLimited);

implementation

end.
