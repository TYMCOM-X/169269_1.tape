comment
! externel definitions for NBSMEM ;

external procedure InitBuffer;

external procedure DumpBuffer( procedure Where );

external procedure SinkByte( reference integer Byte );

external procedure SinkWord( reference integer Word );

external integer procedure SourceByte;
    