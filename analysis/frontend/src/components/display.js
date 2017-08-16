import React from 'react';
import { Chart, Line } from 'react-chartjs-2';
import axios from 'axios';
import _ from 'lodash';
import styled from 'styled-components';

import verticalLinePlugin from './verticalLinePlugin';

const StyledProfit = styled.h2`
  span {
    color: ${props => (props.profit < 0 && props.status === 'Looking To Sell') ? '#ff0000' : '#008000'};
  }
`;

const initialState = {
  currentID: 0,
  status: 'Looking To Buy',
  boughtAt: 0,
  currentPrice: 0,
  profit: 0,
  numTrades: 0,
  numCandles: 0,
  verticalLines: [],
  labels: [],
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
    Chart.pluginService.register({
      afterDatasetsDraw: function (chart, easing) {
        if (chart.config.data.verticalLines) {
            chart.config.data.verticalLines.forEach(pointIndex => verticalLinePlugin.renderVerticalLine(chart, pointIndex));
        }
      }
    })
	},

	componentDidMount(){
		var _this = this;

		setInterval(function(){
      axios.get('http://localhost:3001/newdata').then(response => _this.setNewState(response.data[0]));
		}, 180000);
	},

  resetState() {
    const newDatasets = _.map(this.state.datasets, dataset =>{
      return ({
      ...dataset,
      data: []})})
    this.setState({...initialState, datasets: newDatasets});
  },

  setNewState(candle){
    const label = new Date(candle.timestamp);
    const long = candle.long;
    const short = candle.short;
    const price = candle.price;
    const runID = candle.runID;
    const action = candle.action;

    if (this.state.currentID !== runID) {
      this.resetState();
    }

    const newLabels = this.state.labels.concat([label])
    const oldDataSets = this.state.datasets.slice();

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

    let newProfit;
    let newBoughtAt;
    let numTrades;
    let status;
    var vertLines = this.state.verticalLines;

    if (action === 'hold') {
      newProfit = this.state.profit
      newBoughtAt = this.state.boughtAt
      numTrades = this.state.numTrades;
      status = this.state.status
    } else {
      if (action === 'sold') {
        newProfit = price - this.state.boughtAt;
        newBoughtAt = this.state.boughtAt;
        numTrades = this.state.numTrades + 1;
        vertLines = vertLines.concat([this.state.numCandles]);
        status = 'Looking To Buy'
      } else {
        newProfit = this.state.profit;
        newBoughtAt = price;
        numTrades = this.state.numTrades;
        vertLines = vertLines.concat([this.state.numCandles]);
        status = 'Looking To Sell'
      }
    }

    const newState = {
      status: status,
      boughtAt: newBoughtAt,
      profit: newProfit,
      currentPrice: price,
      numTrades: numTrades,
      currentID: runID,
      labels: newLabels,
      datasets: newDataSets,
      numCandles: this.state.numCandles + 1,
      verticalLines: vertLines,
    };

    this.setState(newState);
  },
	render() {
		return (
      <div>
			<Line
        data={this.state}
        options={{
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
      <StyledProfit
        profit={this.state.currentPrice - this.state.boughtAt}
        status={this.state.status}>
        Current Run Status: <span>
          {(this.state.status === 'Looking To Sell') ? `$${(this.state.currentPrice - this.state.boughtAt).toFixed(2)}` : 'Not in the game'}
        </span>
      </StyledProfit>
      <StyledProfit profit={this.state.profit} status="Looking To Sell">
        Profit: <span>${(this.state.profit).toFixed(2)}</span>
      </StyledProfit>
      <h2>Number of Trades: {this.state.numTrades}</h2>
    </div>
		);
	}
});




export default React.createClass({
  displayName: 'BeginnersLuckData',

  render() {
    return (
      <div>
        <h2>Beginner's Luck Data</h2>
        <p>
          As a proof of concept, we have the bot tell us when it would buy and when it would sell.
          Profit is calculated by taking the price it would have sold at and subtracting the price at which it
          would have been bought.
        </p>
        <Graph />
      </div>
    );
  }
});
