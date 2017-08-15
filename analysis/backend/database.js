import express from 'express';
import Promise from 'bluebird';
import db from 'sqlite';

const app = express();
const port = process.env.PORT || 3001;

app.use(function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
  next();
});

app.get('/', async (req, res, next) => {
  try {
    const [data] = await Promise.all([
      db.all('SELECT * FROM database')
    ]);
    res.send(data);
  } catch (err) {
    next(err);
  }
});

app.get('/newdata', async (req, res, next) => {
  try {
    const [data] = await Promise.all([
      db.all('SELECT * FROM database WHERE timestamp = (SELECT MAX(timestamp) FROM database)')
    ]);
    res.send(data);
  } catch (err) {
    next(err);
  }
});

Promise.resolve()
  // First, try connect to the database
  .then(() => db.open('../../bot/database.db', { Promise }))
  .catch(err => console.error(err.stack))
  // Finally, launch Node.js app
  .finally(() => app.listen(port));
