import React, { Component } from 'react';
import styled from 'styled-components';

import Display from './components/display';

const Wrapper = styled.div`
  max-width: 1800px;
  margin: auto;
`

const Header = styled.div`
  margin: 70px 20px 30px;
  font-family: sans-serif;
  font-size: xx-large;
`;

const Line = styled.div`
  height: 1px;
  width: 100px;
  border-top: #000 solid 2px;
  margin-left: 20px;
`;

const Info = styled.div`
  font-family: sans-serif;
  font-size: medium;
  margin: 30px 20px 50px;
  max-width: 825px;
`;

const Container = styled.div`
  position: relative;
  margin-right: 20px;
`;

class App extends Component {
  render() {
    return (
      <Wrapper className="App">
        <Header>Beginner's Luck Data</Header>
        <Line />
        <Info>
          As a proof of concept, we have the bot tell us when it would buy and when it would sell.
          Profit is calculated by taking the price it would have sold at and subtracting the price at which it
          would have been bought.
        </Info>
        <Container>
          <Display />
        </Container>
      </Wrapper>
    );
  }
}

export default App;
