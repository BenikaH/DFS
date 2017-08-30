var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');
var path = 'scrape.html'

page.open('https://www.fantasypros.com/nfl/auction-values/calculator.php', function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});