import React from 'react';
import { Chart, Line } from 'react-chartjs-2';
import axios from 'axios';
import _ from 'lodash';
import styled from 'styled-components';

import verticalLinePlugin from './verticalLinePlugin';

const StyledProfit = styled.h2`
  span {
    color: ${props => (props.profit < 0) ? '#ff0000' : '#008000'};
  }
`;

const Wrapper = styled.div`
  margin: 50px 50px;
  font-family: sans-serif;
  font-size: large;
  display: flex;
  justify-content: space-between;
`;

const Container = styled.div`
`;

const Title = styled.div`
  font-size: small;
  color: #8f8f8f;
`;

const Info = styled.div`
  font-weight: bold;
  margin: 30px 0;

  p {
    color: #31e2c7;
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
      label: 'Long EMA',
      fill: false,
      lineTension: 0.1,
      backgroundColor: 'rgba(108, 168, 198, 0.4)',
      borderColor: 'rgba(108, 168, 198, 1)',
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: 'rgba(108, 168, 198, 1)',
      pointBackgroundColor: '#fff',
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: 'rgba(108, 168, 198, 1)',
      pointHoverBorderColor: 'rgba(220,220,220,1)',
      pointHoverBorderWidth: 2,
      pointRadius: 1,
      pointHitRadius: 10,
      data: []
    },
    {
      label: 'Short EMA',
      fill: false,
      lineTension: 0.1,
      backgroundColor: 'rgba(222, 99, 163, 0.4)',
      borderColor: 'rgba(222, 99, 163, 1)',
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: 'rgba(222, 99, 163, 1)',
      pointBackgroundColor: '#fff',
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: 'rgba(222, 99, 163, 1)',
      pointHoverBorderColor: 'rgba(222, 99, 163, 1)',
      pointHoverBorderWidth: 2,
      pointRadius: 1,
      pointHitRadius: 10,
      data: []
    },
    {
      label: 'Price',
      fill: true,
      lineTension: 0.1,
      backgroundColor: 'rgba(83, 184, 165, 1)',
      borderColor: 'rgba(83, 184, 165, 1)',
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: 'rgba(83, 184, 165, 1)',
      pointBackgroundColor: '#fff',
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: 'rgba(83, 184, 165, 1)',
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
      },
      beforeDraw: function (chart, easing) {
        if (chart.config.options.chartArea && chart.config.options.chartArea.backgroundColor) {
            var helpers = Chart.helpers;
            var ctx = chart.chart.ctx;
            var chartArea = chart.chartArea;

            ctx.save();
            ctx.fillStyle = chart.config.options.chartArea.backgroundColor;
            ctx.fillRect(chartArea.left, chartArea.top, chartArea.right - chartArea.left, chartArea.bottom - chartArea.top);
            ctx.restore();
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
      if (oldDataSet.label === 'Short EMA') {
        data = short
      } else if (oldDataSet.label === 'Long EMA') {
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
          chartArea: {
            backgroundColor: 'rgba(78, 78, 78, 1)'
          },
          scales: {
            xAxes: [
              {
                type: 'time',
                time: {
                  unit: 'minute',
                  tooltipFormat: 'll HH:mm',
                  displayFormats: {
                    millisecond: 'h:mm a',
                    second: 'h:mm a',
                    minute: 'h:mm a',
                    hour: 'h:mm a',
                    day: 'll h:mm a',
                  },
                },
                scaleLabel: {
                  display: true,
                  labelString: 'Timestamp',
                  fontStyle: 'bold',
                  fontColor: '#000'
                },
                gridLines: {
                  display: false,
                }
              }
            ],
            yAxes: [
              {
                gridLines:{
                  color: 'rgba(145, 143, 143, 1)'
                },
                scaleLabel: {
                  display: true,
                  labelString: 'Eth Value (USD)',
                  fontStyle: 'bold',
                  fontColor: '#000'
                }
              }
            ]
          }
      }} />
      <Wrapper>
        <Container>
          <Title>
            Status
          </Title>
          <Info>
            {this.state.status}
          </Info>
        </Container>
        <Container>
          <Title>Current Run Status</Title>
          <Info><p>{(this.state.status === 'Looking To Sell') ? `$${(this.state.currentPrice - this.state.boughtAt).toFixed(2)}` : 'Not in the game'}</p></Info>
        </Container>
        <Container>
          <Title>Net Profit/Loss</Title>
          <Info><p>${(this.state.profit).toFixed(2)}</p></Info>
        </Container>
        <Container>
          <Title>Number of Trades</Title>
          <Info>{this.state.numTrades}</Info>
        </Container>
      </Wrapper>
    </div>
		);
	}
});




export default React.createClass({
  displayName: 'BeginnersLuckData',

  render() {
    return (
      <div>
        <Graph />
      </div>
    );
  }
});
