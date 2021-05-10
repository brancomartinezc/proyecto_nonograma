import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Mode from './Mode';

class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      grid: null,
      rowClues: null,
      colClues: null,
      filaSat: null,
      colSat: null,
      waiting: false,
      mode: "#",
      /*pFila: null,
      grupos: null,
      roww: null,
      rowstr: null,*/
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
    this.modeClick = this.modeClick.bind(this);
  }

  handlePengineCreate() {
    const queryS = 'init(PistasFilas, PistasColumns, Grilla)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grilla'],
          rowClues: response['PistasFilas'],
          colClues: response['PistasColumns'],
        });
      }
    });
  }

  handleClick(i, j) {
    // No action on click if we are waiting.
    if (this.state.waiting) {
      return;
    }
    
    const squaresS = JSON.stringify(this.state.grid).replaceAll('"_"', "_"); // Remove quotes for variables.
    const filas = JSON.stringify(this.state.rowClues)
    const columnas = JSON.stringify(this.state.colClues);
    
    // Build Prolog query to make the move, which will look as follows:
    // put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    const queryS = `put("${this.state.mode}", [${i}, ${j}], ${filas}, ${columnas}, ${squaresS}, GrillaRes, FilaSat, ColSat)`;
    console.log(queryS); //debugger

    this.setState({
      waiting: true
    });
    this.pengine.query(queryS, (success, response) => {
      // console.log(response); // DEBUG
      if (success) {
        this.setState({
          grid: response['GrillaRes'],
          filaSat: response['FilaSat'],
          colSat: response['ColSat'],
          /*pFila: response['PFila'],
          grupos: ['Grupos'],
          roww: response['Row'],
          rowstr: response['RowString'],*/
          waiting: false
        });
      } else {
        this.setState({
          waiting: false
        });
      }
    });
    
  }

  modeClick(){
    if(this.state.mode === "#"){
      this.setState({ mode: "X" });
    }else{
      this.setState({ mode: "#" });
    }
  }

  render() {
    if (this.state.grid === null) {
      return null;
    }
    
    console.log('Grid nueva: ' + this.state.grid); // DEBUG
    console.log('FilaSat: ' + this.state.filaSat); // DEBUG
    console.log('ColSat: ' + this.state.colSat);   // DEBUG

    // const statusText = 'Keep playing!';

    return (
      <div className="game">
        <Board
          grid={this.state.grid}
          rowClues={this.state.rowClues}
          colClues={this.state.colClues}
          onClick={(i, j) => this.handleClick(i,j)}
        />
        <div>
          Modo actual: <Mode value={this.state.mode} classN="square" onClick={() => this.modeClick()}/>
        </div>
        {/*<div className="gameInfo">
          {statusText}
        </div>*/}
      </div>
    );
  }
}

export default Game;
