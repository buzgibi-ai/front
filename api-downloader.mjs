import * as https from 'https'; // or 'https' for https:// URLs
import * as fs from 'fs';

process.env['NODE_TLS_REJECT_UNAUTHORIZED'] = 0;

var url = process.argv.slice(2)[0];
var target = process.argv.slice(2)[1];
var cred = process.argv.slice(2)[2];

console.log('swagger host: ' + url + ' to file: ' + target);

const file = fs.createWriteStream(target);

var options = {
    host: url,
    port: 443,
    path: '/swagger',
    // authentication headers
    headers: {
       'Authorization': 'Basic ' + cred
    }   
 };

https.get(options, function(response) {
   response.pipe(file);

   // after download completed close file stream
   file.on("finish", () => {
       file.close();
       console.log("Download Completed");
   });
});