import React from 'react';
import { Chart, Line } from 'react-chartjs-2';
import axios from 'axios';
import _ from 'lodash';
import styled from 'styled-components';

import verticalLinePlugin from './verticalLinePlugin';

const priceColor = 'rgba(117, 117, 117, 1)';
const shortColor = 'rgba(133, 122, 187, 1)';
const longColor = 'rgba(76, 195, 138, 1)';
const boughtColor = 'rgba(140, 186, 240, 1)';
const soldColor = 'rgba(182, 103, 160, 1)';

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
      backgroundColor: 'rgba(76, 195, 138, 0.4)',
      borderColor: longColor,
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: longColor,
      pointBackgroundColor: longColor,
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: longColor,
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
      backgroundColor: 'rgba(133, 122, 187, 0.4)',
      borderColor: shortColor,
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: shortColor,
      pointBackgroundColor: shortColor,
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: shortColor,
      pointHoverBorderColor: 'rgba(220,220,220,1)',
      pointHoverBorderWidth: 2,
      pointRadius: 1,
      pointHitRadius: 10,
      data: []
    },
    {
      label: 'Price',
      fill: true,
      lineTension: .1,
      backgroundColor: priceColor,
      borderColor: priceColor,
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: priceColor,
      pointBackgroundColor: priceColor,
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: priceColor,
      pointHoverBorderColor: 'rgba(220,220,220,1)',
      pointHoverBorderWidth: 2,
      pointRadius: 1,
      pointHitRadius: 10,
      data: []
    },
    {
      label: 'Bought',
      fill: false,
      lineTension: 0.1,
      backgroundColor: 'rgba(140, 186, 240, 0.4)',
      borderColor: boughtColor,
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: boughtColor,
      pointBackgroundColor: boughtColor,
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: boughtColor,
      pointHoverBorderColor: 'rgba(220,220,220,1)',
      pointHoverBorderWidth: 2,
      pointRadius: 1,
      pointHitRadius: 10,
      data: []
    },
    {
      label: 'Sold',
      fill: false,
      lineTension: 0.1,
      backgroundColor: 'rgba(182, 103, 160, 0.4)',
      borderColor: soldColor,
      borderCapStyle: 'butt',
      borderDash: [],
      borderDashOffset: 0.0,
      borderJoinStyle: 'miter',
      pointBorderColor: soldColor,
      pointBackgroundColor: soldColor,
      pointBorderWidth: 1,
      pointHoverRadius: 5,
      pointHoverBackgroundColor: soldColor,
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
        const active = {
          bought: true,
          sold: true,
        }
        chart.legend.legendItems.forEach(leg => {
          if (leg.text ==='Sold' && leg.hidden === true) {
            active.sold = false
          } else if (leg.text === 'Bought' && leg.hidden === true) {
            active.bought = false
          }
        });
        if (chart.config.data.verticalLines) {
            chart.config.data.verticalLines.forEach(pointer => verticalLinePlugin.renderVerticalLine(chart, pointer, active));
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
      if (data) {
        const newData = oldDataSet.data.concat([data.toFixed(2)])
        return {...oldDataSet, data: newData}
      }
      return {...oldDataSet, data: oldDataSet.data.concat([data])}
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
        newProfit = (price - this.state.boughtAt).toFixed(2);
        newBoughtAt = this.state.boughtAt;
        numTrades = this.state.numTrades + 1;
        vertLines = vertLines.concat([[this.state.numCandles, 'sold']]);
        status = 'Looking To Buy'
      } else {
        newProfit = this.state.profit;
        newBoughtAt = price;
        numTrades = this.state.numTrades;
        vertLines = vertLines.concat([[this.state.numCandles, 'bought']]);
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
          title: {
            display: true,
            fontStyle: 'bold',
            text: 'ETH Value over Time',
            fontColor: '#000',
            fontSize: 30
          },
          chartArea: {
            backgroundColor: 'rgba(78, 78, 78, 1)'
          },
          scales: {
            xAxes: [
              {
                type: 'time',
                time: {
                  unit: 'minute',
                  tooltipFormat: 'll h:mm a',
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
                  labelString: 'ETH Value (USD)',
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
          <Info><p>{(this.state.status === 'Looking To Sell') ? `$${this.state.currentPrice - this.state.boughtAt}` : 'Not in the game'}</p></Info>
        </Container>
        <Container>
          <Title>Net Profit/Loss</Title>
          <Info><p>${this.state.profit}</p></Info>
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
