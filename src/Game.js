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
      waiting: false,
      mode: "#",
      filasCorrectas: [],
      colsCorrectas: [],
      gameWon: false,
      statusText: "Juego en progreso."
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
        
        //inicializacion de arreglos de control de filas y columnas correctas.
        this.state.grid.forEach(() => {
          this.state.filasCorrectas.push(0);
        });

        this.state.grid[0].forEach(() => {
          this.state.colsCorrectas.push(0);
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
    const filas = JSON.stringify(this.state.rowClues);
    const columnas = JSON.stringify(this.state.colClues);
    
    // Build Prolog query to make the move, which will look as follows:
    // put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    const queryS = `put("${this.state.mode}", [${i}, ${j}], ${filas}, ${columnas}, ${squaresS}, GrillaRes, FilaSat, ColSat)`;
    //console.log(queryS); //DEBUG

    this.setState({
      waiting: true,
    });
    this.pengine.query(queryS, (success, response) => {
      
      let auxFilas = this.state.filasCorrectas.slice();
      auxFilas[i] = response['FilaSat'];
      let auxCols = this.state.colsCorrectas.slice();
      auxCols[j] = response['ColSat'];

      if (success) {
        this.setState({
          grid: response['GrillaRes'],
          filasCorrectas: auxFilas,
          colsCorrectas: auxCols,
          waiting: false
        });
        
        //control de victoria, si todas las filas y columnas son correctas gano.
        let todasFilasCorrectas = this.state.filasCorrectas.every(elem => elem === 1);
        let todasColsCorrectas = this.state.colsCorrectas.every(elem  => elem === 1);
        
        if(todasColsCorrectas && todasFilasCorrectas){
          this.setState({
            gameWon: true,
            statusText: "Ganaste!"
          }) 
        }

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
    
    return (
      <div className="game">
        <h1>Nonograma</h1>
        <div>
          Modo actual: <Mode value={this.state.mode} gameWon={this.state.gameWon} onClick={() => this.modeClick()}/>
        </div>
        {<div className="gameInfo">
          {this.state.statusText}
        </div>}

        <Board
          grid={this.state.grid}
          rowClues={this.state.rowClues}
          colClues={this.state.colClues}
          gameWon={this.state.gameWon}
          filasCorrectas={this.state.filasCorrectas}
          columnasCorrectas={this.state.colsCorrectas}
          onClick={(i, j) => this.handleClick(i,j)}
        />
      </div>
    );
  }
}

export default Game;
