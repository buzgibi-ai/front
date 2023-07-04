import * as https from 'https'; // or 'https' for https:// URLs
import * as fs from 'fs';

process.env['NODE_TLS_REJECT_UNAUTHORIZED'] = 0;

var url = process.argv.slice(2)[0];
var target = process.argv.slice(2)[1];

console.log('swagger host: ' + url + ' to file: ' + target);

const file = fs.createWriteStream(target);
https.get(url, function(response) {
   response.pipe(file);

   // after download completed close file stream
   file.on("finish", () => {
       file.close();
       console.log("Download Completed");
   });
});