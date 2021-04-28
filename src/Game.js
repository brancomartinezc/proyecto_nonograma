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
      waiting: false,
      mode: "#",
      pistas_filas: null,
      pistas_columnas: null
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
          pistas_filas: response['PistasFilas'],
          pistas_columnas: response['PistasColumns']
        });
        
        console.log("Columnas");
        this.state.pistas_columnas.forEach((item) => {
          console.log(item)
        });
        console.log("~~~~~~~~~");
        console.log("Filas");
        this.state.pistas_filas.forEach((item) => {
          console.log(item)
        });
      }
    });
  }

  handleClick(i, j) {
    // No action on click if we are waiting.
    if (this.state.waiting) {
      return;
    }
    // Build Prolog query to make the move, which will look as follows:
    // put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    const squaresS = JSON.stringify(this.state.grid).replaceAll('"_"', "_"); // Remove quotes for variables.
    const filas = JSON.stringify(this.state.pistas_filas)
    const columnas = JSON.stringify(this.state.pistas_columnas);
    
    const queryS = `put("${this.state.mode}", [${i}, ${j}], ${filas}, ${columnas}, ${squaresS}, GrillaRes, FilaSat, ColSat)`;
    console.log(queryS); //debugger

    this.setState({
      waiting: true
    });
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['GrillaRes'],
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
    // const statusText = 'Keep playing!';
    return (
      <div className="game">
        <Board
          grid={this.state.grid}
          pistas_columnas={this.state.pistas_columnas}
          pistas_filas={this.state.pistas_filas}
          onClick={(i, j) => this.handleClick(i,j)}
        />
        <div>
          Modo actual: <Mode value={this.state.mode} classN="square" onClick={() => this.modeClick()}/>
        </div>
        {/* <div className="gameInfo">
          {statusText}
        </div> */}
      </div>
    );
  }
}

export default Game;
