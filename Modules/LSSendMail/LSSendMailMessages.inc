  SLSAboutMsg_rst =
    'LazSolutions, Send mail module' + LineEnding +
    'Copyright (C) 2011-2013 Silvio Clecio.' + LineEnding + LineEnding +
    'http://silvioprog.com.br' + LineEnding + LineEnding +
    'See the file LGPL.2.1.txt, included in this distribution,' + LineEnding +
    'for details about the copyright.' + LineEnding + LineEnding +
    'This library is distributed in the hope that it will be useful,' + LineEnding +
    'but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding +
    'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.';
  SLSHelpMsg_rst =
{$IFDEF UNIX}
    'Available parameters:' + LineEnding +
    '  --about,           Show information about the project.' + LineEnding +
    '  -h, --help,        Show this text.' + LineEnding +
    '  --checkopenssl,    Show if have SSL support.' + LineEnding +
    '  -debugmode,        Debug mode shows information of MIME without send e-mail, e.g: -debugmode=debug.txt' + LineEnding +
    '  -attempt,          Define all attemps to try send mail, e.g: -attempt=''3''. Default is 3.' + LineEnding +
    '  -from,             Set <From> (may be your name).' + LineEnding +
    '  -to,               Set <To> destinations, e.g: -to=''mail1@host.com;mail2@host.com''.' + LineEnding +
    '  -cc,               Set <CC> destinations, e.g: -cc=''mail1@host.com;mail2@host.com''.' + LineEnding +
    '  -bcc               Set <BCC> destinations, e.g: -bcc=''mail1@host.com;mail2@host.com''.' + LineEnding +
    '  -subject,          Set <Subject> string.' + LineEnding +
    '  -message,          Set <Message> string. You can also encode strings in URL format, e.g: Silvio%20Cl%C3%A9cio = Silvio Clécio.' + LineEnding +
    '  -messagefile,      Set <Message> string from file, e.g: -messagefile=''message.txt''' + LineEnding +
    '  -attached,         Set <Attached>. To send more than one attached, separate by semicolons, e.g: -attached=''attached1.txt;attached2.txt''.' + LineEnding +
    '  -messagetype,      Set message type, e.g: -messagetype=txt for TXT message, or -messagetype=html for HTML message.' + LineEnding +
    '  -priority,         Set the priority option, use the following options: unknown, low, normal, high. Unknown is default.' + LineEnding +
    '  -confirmreading,   Set confirmation to reading for your e-mail. Use y to True, or n to False.' + LineEnding +
    '  -user,             Set SMTP user.' + LineEnding +
    '  -password,         Set SMTP password.' + LineEnding +
    '  -host,             Set SMTP host.' + LineEnding +
    '  -port,             Set SMTP port.' + LineEnding +
    '  -ssl,              Set SMTP SSL. Use y to True, or n to False.' + LineEnding +
    '  -tls,              Set SMTP TLS. Use y to True, or n to False.' + LineEnding +
    '  --version,         Show project version.' + LineEnding + LineEnding +
    'To see more details or report bugs, visit:' + LineEnding + LineEnding +
    '<https://github.com/silvioprog/lazsolutions>' + LineEnding + LineEnding +
    'Enjoy! :)';
{$ELSE}
    'Available parameters:' + LineEnding +
    '  --about,           Show information about the project.' + LineEnding +
    '  -h, --help,        Show this text.' + LineEnding +
    '  --checkopenssl,    Show if have SSL support.' + LineEnding +
    '  -debugmode,        Debug mode shows information of MIME without send e-mail, e.g: -debugmode=debug.txt' + LineEnding +
    '  -attempt,          Define all attemps to try send mail, e.g: -attempt="3". Default is 3.' + LineEnding +
    '  -from,             Set <From> (may be your name).' + LineEnding +
    '  -to,               Set <To> destinations, e.g: -to="mail1@host.com;mail2@host.com".' + LineEnding +
    '  -cc,               Set <CC> destinations, e.g: -cc="mail1@host.com;mail2@host.com".' + LineEnding +
    '  -bcc               Set <BCC> destinations, e.g: -bcc="mail1@host.com;mail2@host.com".' + LineEnding +
    '  -subject,          Set <Subject> string.' + LineEnding +
    '  -message,          Set <Message> string. You can also encode strings in URL format, e.g: Silvio%20Cl%C3%A9cio = Silvio Clécio.' + LineEnding +
    '  -messagefile,      Set <Message> string from file, e.g: -messagefile="message.txt"' + LineEnding +
    '  -attached,         Set <Attached>. To send more than one attached, separate by semicolons, e.g: -attached="attached1.txt;attached2.txt".' + LineEnding +
    '  -messagetype,      Set message type, e.g: -messagetype=txt for TXT message, or -messagetype=html for HTML message.' + LineEnding +
    '  -priority,         Set the priority option, use the following options: unknown, low, normal, high. Unknown is default.' + LineEnding +
    '  -confirmreading,   Set confirmation to reading for your e-mail. Use y to True, or n to False.' + LineEnding +
    '  -user,             Set SMTP user.' + LineEnding +
    '  -password,         Set SMTP password.' + LineEnding +
    '  -host,             Set SMTP host.' + LineEnding +
    '  -port,             Set SMTP port.' + LineEnding +
    '  -ssl,              Set SMTP SSL. Use y to True, or n to False.' + LineEnding +
    '  -tls,              Set SMTP TLS. Use y to True, or n to False.' + LineEnding +
    '  --version,         Show project version.' + LineEnding + LineEnding +
    'To see more details or report bugs, visit:' + LineEnding + LineEnding +
    '<https://github.com/silvioprog/lazsolutions>' + LineEnding + LineEnding +
    'Enjoy! :)';
{$ENDIF}
  SLSInsufficientParameters_rst = 'You must complete at least the parameters ' +
    '"-to", "-user", "-password", "-host" and "-port".';
  SLSParamEmptyError_rst = 'Parameter "%s" can not be empty.';
  SLSUnrecognizedParamError_rst = 'Unrecognized param: "%s".';
  SLSUnknownError_rst = 'Unknown error.';
