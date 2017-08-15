import React from 'react';
import { Line } from 'react-chartjs-2';
import axios from 'axios';
import _ from 'lodash';

const initialState = {
  currentID: 0,
  status: 'LookingTo Buy',
  boughtAt: 0,
  profit: 0,
  numTrades: 0,
  labels: [],
  numberTrades: 0,
  datasets: [
    {
      label: 'Long SMA',
      fill: false,
      lineTension: 0.1,
      backgroundColor: 'rgba(75,192,192,0.4)',
      borderColor: 'rgba(75,192,192,1)',
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: 'rgba(75,192,192,1)',
      pointBackgroundColor: '#fff',
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: 'rgba(75,192,192,1)',
      pointHoverBorderColor: 'rgba(220,220,220,1)',
      pointHoverBorderWidth: 2,
      pointRadius: 1,
      pointHitRadius: 10,
      data: []
    },
    {
      label: 'Short SMA',
      fill: false,
      lineTension: 0.1,
      backgroundColor: 'rgba(180, 58, 146, 0.4)',
      borderColor: 'rgba(180, 58, 146, 1)',
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: 'rgba(180, 58, 146, 1)',
      pointBackgroundColor: '#fff',
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: 'rgba(180, 58, 146, 1)',
      pointHoverBorderColor: 'rgba(220,220,220,1)',
      pointHoverBorderWidth: 2,
      pointRadius: 1,
      pointHitRadius: 10,
      data: []
    },
    {
      label: 'Price',
      fill: false,
      lineTension: 0.1,
      backgroundColor: 'rgba(76, 158, 59, 0.4)',
      borderColor: 'rgba(76, 158, 59, 1)',
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: 'rgba(76, 158, 59, 1)',
      pointBackgroundColor: '#fff',
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: 'rgba(76, 158, 59, 1)',
      pointHoverBorderColor: 'rgba(220,220,220,1)',
      pointHoverBorderWidth: 2,
      pointRadius: 1,
      pointHitRadius: 10,
      data: []
    }
  ]
};

const Graph = React.createClass({
	displayName: 'Graph',
	componentWillMount(){
		this.setState(initialState);
    axios.get('http://localhost:3001').then(response => _.map(response.data, this.setNewState));

	},
	componentDidMount(){
		var _this = this;

		setInterval(function(){
      axios.get('http://localhost:3001/newdata').then(response => _this.setNewState(response.data[0]));
		}, 180000);
	},
  setNewState(candle){
    const label = candle.timestamp.slice(0, 19);
    const long = candle.long;
    const short = candle.short;
    const price = candle.price;
    const runID = candle.runID;
    const status = candle.position;
    /*
    if (this.state.currentID !== runID) {
      this.setState(initialState);
    }
    */
    const newLabels = this.state.labels.concat([label])
    const oldDataSets = this.state.datasets;

    var newDataSets = _.map(oldDataSets, oldDataSet => {
      let data;
      if (oldDataSet.label === 'Short SMA') {
        data = short
      } else if (oldDataSet.label === 'Long SMA') {
        data = long
      } else if (oldDataSet.label === 'Price') {
        data = price
      }

      const newData = oldDataSet.data.concat([data])
      return {...oldDataSet, data: newData}
    })

    let newProfit
    let newBoughtAt
    let numTrades
    if (this.state.status === status) {
      newProfit = this.state.profit
      newBoughtAt = this.state.boughtAt
      numTrades = this.state.numTrades;
    } else {
      if (this.state.status === 'LookingTo Sell') {
        newProfit = price - this.state.boughtAt;
        newBoughtAt = this.state.boughtAt;
        numTrades = this.state.numTrades + 1;
      } else {
        newProfit = this.state.profit;
        newBoughtAt = price;
        numTrades = this.state.numTrades;
      }
    }

    const newState = {
      status: status,
      boughtAt: newBoughtAt,
      profit: newProfit,
      numTrades: numTrades,
      currentID: runID,
      labels: newLabels,
      datasets: newDataSets
    };

    this.setState(newState);
  },
	render() {
		return (
      <div>
			<Line data={this.state} options={{
        scales: {
          xAxes: [
            {
              type: 'time',
              time: {
                tooltipFormat: 'll HH:mm',
                displayFormat: 'll HH:mm:'
              },
              scaleLabel: {
                display: true,
                labelString: 'Timestamp'
              },
              ticks: {
                minRotation: 70,
              }
            }
          ],
          yAxes: [
            {
              scaleLabel: {
                display: true,
                labelString: 'Eth Value (USD)'
              }
            }
          ]
        }
      }} />
      <h2>Status: {this.state.status}</h2>
      <h2>Profit: ${this.state.profit}</h2>
      <h2>Number of Trades: {this.state.numTrades}</h2>
    </div>
		);
	}
});




export default React.createClass({
  displayName: 'RandomizedDataLineExample',

  render() {
    return (
      <div>
        <h2>Beginner's Luck Data</h2>
        <p>
          As a proof of concept, we have the bot tell us when it would buy and when it would sell.
          Profit is calculated by taking the price it sold at and subtracting the price when it
          would have been bought.
        </p>
        <Graph />
      </div>
    );
  }
});
