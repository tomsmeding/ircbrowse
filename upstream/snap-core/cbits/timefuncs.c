#define _DEFAULT_SOURCE
#define _XOPEN_SOURCE
#define _BSD_SOURCE
#include <stdio.h>
#include <time.h>
#include <locale.h>


void set_c_locale() {
    setlocale(LC_TIME, "C");
}


time_t c_parse_http_time(char* s) {
	fprintf(stderr, "c_parse_http_time: yo!\n");

	if (s == NULL) {
		fprintf(stderr, "c_parse_http_time: s == NULL\n");
	}

	// Thu, 01 Jan 1970 00:00:10 GMT
	// 01234567890123456789012345678
	//          10        20      28
	if (s[28] != '\0') {
		fprintf(stderr, "c_parse_http_time: s[28] = %u\n", (unsigned)s[28]);
	}

    struct tm dest;
    strptime(s, "%a, %d %b %Y %H:%M:%S GMT", &dest);
    return timegm(&dest);
}

void c_format_http_time(time_t src, char* dest) {
    struct tm t;
    gmtime_r(&src, &t);
    strftime(dest, 40, "%a, %d %b %Y %H:%M:%S GMT", &t);
}

void c_format_log_time(time_t src, char* dest) {
    struct tm t;
    localtime_r(&src, &t);
    strftime(dest, 40, "%d/%b/%Y:%H:%M:%S %z", &t);
}
