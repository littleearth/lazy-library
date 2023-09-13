{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Types
  Author: Tristan Marlow
  Purpose: Generic platform utilties

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions

  ----------------------------------------------------------------------------; }
unit Lazy.Utils;

interface

uses
  Lazy.Types, Lazy.Utils.Base;

// File and Folder Routines

type
  TLZFile = class(TLZFileBase);
  TLZSystem = class(TLZSystemBase);
  TLZString = class(TLZStringBase);
  TLZDateTime = class(TLZDateTimeBase);
  TLZBoolean = class(TLZBooleanBase);
  TLZMath = class(TLZMathBase);

implementation

end.
