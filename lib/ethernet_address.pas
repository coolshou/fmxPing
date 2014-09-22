unit ethernet_address;

interface

uses classes, sysutils;

const
     MAX_INTERFACE_NAME_LEN             = $100;
     ERROR_SUCCESS                      = 0;
     MAXLEN_IFDESCR                     = $100;
     MAXLEN_PHYSADDR                    = 8;

     MIB_IF_OPER_STATUS_NON_OPERATIONAL = 0 ;
     MIB_IF_OPER_STATUS_UNREACHABLE     = 1;
     MIB_IF_OPER_STATUS_DISCONNECTED    = 2;
     MIB_IF_OPER_STATUS_CONNECTING      = 3;
     MIB_IF_OPER_STATUS_CONNECTED       = 4;
     MIB_IF_OPER_STATUS_OPERATIONAL     = 5;

     MIB_IF_TYPE_OTHER                  = 1;
     MIB_IF_TYPE_ETHERNET               = 6;
     MIB_IF_TYPE_TOKENRING              = 9;
     MIB_IF_TYPE_FDDI                   = 15;
     MIB_IF_TYPE_PPP                    = 23;
     MIB_IF_TYPE_LOOPBACK               = 24;
     MIB_IF_TYPE_SLIP                   = 28;

     MIB_IF_ADMIN_STATUS_UP             = 1;
     MIB_IF_ADMIN_STATUS_DOWN           = 2;
     MIB_IF_ADMIN_STATUS_TESTING        = 3;


type

   MIB_IFROW            = Record
     wszName : Array[0 .. (MAX_INTERFACE_NAME_LEN*2-1)] of char;
     dwIndex              : LongInt;
     dwType               : LongInt;
     dwMtu                : LongInt;
     dwSpeed              : LongInt;
     dwPhysAddrLen        : LongInt;
     bPhysAddr : Array[0 .. (MAXLEN_PHYSADDR-1)] of Byte;
     dwAdminStatus        : LongInt;
     dwOperStatus         : LongInt;
     dwLastChange         : LongInt;
     dwInOctets           : LongInt;
     dwInUcastPkts        : LongInt;
     dwInNUcastPkts       : LongInt;
     dwInDiscards         : LongInt;
     dwInErrors           : LongInt;
     dwInUnknownProtos    : LongInt;
     dwOutOctets          : LongInt;
     dwOutUcastPkts       : LongInt;
     dwOutNUcastPkts      : LongInt;
     dwOutDiscards        : LongInt;
     dwOutErrors          : LongInt;
     dwOutQLen            : LongInt;
     dwDescrLen           : LongInt;
     bDescr     : Array[0 .. (MAXLEN_IFDESCR - 1)] of Char;
     end;

function Get_EthernetAddresses: TStringList;

Function GetIfTable( pIfTable : Pointer;
                 VAR pdwSize  : LongInt;
                     bOrder   : LongInt ): LongInt; stdcall;


implementation

Function GetIfTable; stdcall; external 'IPHLPAPI.DLL';

function Get_EthernetAddresses: TStringList;
const
   _MAX_ROWS_ = 20;

type
   _IfTable = Record
                 nRows : LongInt;
                 ifRow : Array[1.._MAX_ROWS_] of MIB_IFROW;
              end;

VAR
   pIfTable  : ^_IfTable;
   TableSize : LongInt;
   tmp       : String;
   i,j       : Integer;
   ErrCode   : LongInt;
begin
   pIfTable := nil;
   //------------------------------------------------------------
   Result:=TStringList.Create;
   if Assigned(Result) then
   try
      //-------------------------------------------------------
      // First: just get the buffer size. 
      // TableSize returns the size needed.
      TableSize:=0; // Set to zero so the GetIfTabel function 
                    // won't try to fill the buffer yet, 
                    // but only return the actual size it needs.
      GetIfTable(pIfTable, TableSize, 1);
      if (TableSize < SizeOf(MIB_IFROW)+Sizeof(LongInt)) then
      begin
         Exit; // less than 1 table entry?!
      end; // if-end.

      // Second: 
      // allocate memory for the buffer and retrieve the 
      // entire table.
      GetMem(pIfTable, TableSize);
      ErrCode := GetIfTable(pIfTable, TableSize, 1);
      if ErrCode<>ERROR_SUCCESS then
      begin
         Exit; // OK, that did not work. 
               // Not enough memory i guess.
      end; // if-end.

      // Read the ETHERNET addresses.
      for i := 1 to pIfTable^.nRows do
      try
         if pIfTable^.ifRow[i].dwType=MIB_IF_TYPE_ETHERNET then
         begin
            tmp:='';
            for j:=0 to pIfTable^.ifRow[i].dwPhysAddrLen-1 do
            begin
               tmp := tmp + format('%.2x', 
                      [ pIfTable^.ifRow[i].bPhysAddr[j] ] );
            end; // for-end.
            //-------------------------------------
            if Length(tmp)>0 then Result.Add(tmp);
         end; // if-end.
      except
         Exit;
      end; // if-try-except-end.
   finally
      if Assigned(pIfTable) then FreeMem(pIfTable,TableSize);
   end; // if-try-finally-end.
end;

// Enjoy! 