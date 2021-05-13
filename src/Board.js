import React from 'react';
import Square from './Square';
import Clue from './Clue';

class Board extends React.Component {
    render() {
        const numOfRows = this.props.grid.length;
        const numOfCols = this.props.grid[0].length;

        const rowClues = this.props.rowClues;
        const colClues = this.props.colClues;

        /*Todavia no se como distinguir cual de todas las clue es la que tiene que pintarse de verde, 
        capaz ese i en el parametro sirve de algo
        */
        const lastRowSat =  ((this.props.lastRowSat === 1) ? true : false);
        const lastColSat = ((this.props.lastColSat === 1) ? true : false);

        const lastRowJugada = this.props.posJugada[0];
        const lastColJugada = this.props.posJugada[1];

        return (
            <div className="vertical">
                <div
                    className="colClues"
                    style={{
                        gridTemplateRows: '60px',
                        gridTemplateColumns: '60px repeat(' + numOfCols + ', 40px)'
                        /*
                           60px  40px 40px 40px 40px 40px 40px 40px   (gridTemplateColumns)
                          ______ ____ ____ ____ ____ ____ ____ ____
                         |      |    |    |    |    |    |    |    |  60px
                         |      |    |    |    |    |    |    |    |  (gridTemplateRows)
                          ------ ---- ---- ---- ---- ---- ---- ---- 
                         */
                    }}
                >
                    <div>{/* top-left corner square */}</div>

                    {/* Col clues */}
                    {colClues.map((clue, i) =>
                        <Clue clue={clue} sat={(i === lastColJugada) && lastColSat} key={i}/>

                        // Restos de debuggear paso de estado entre props
                        // console.log(`i ${i}, lastColJugada: ${lastColJugada}, lastColSat: ${lastColSat}`) && (i === lastColJugada) ?
                        //     <Clue clue={clue} sat={lastColSat} key={i}/>
                        //         :
                        //     <Clue clue={clue} sat={false} key={i}/>
                    )}
                </div>
                <div className="horizontal">
                    <div
                        className="rowClues"
                        style={{
                            gridTemplateRows: 'repeat(' + numOfRows + ', 40px)',
                            gridTemplateColumns: '60px'
                            /* IDEM column clues above */
                        }}
                    >
                        {/* Row clues */}
                        {rowClues.map((clue, i) =>
                            <Clue clue={clue} sat={(i === lastRowJugada) && lastRowSat} key={i}/>
                        )}
                    </div>
                    <div className="board"
                        style={{
                            gridTemplateRows: 'repeat(' + numOfRows + ', 40px)',
                            gridTemplateColumns: 'repeat(' + numOfCols + ', 40px)'
                        }}>
                        {this.props.grid.map((row, i) =>
                            row.map((cell, j) =>
                                <Square
                                    value={cell}
                                    gameWon={this.props.gameWon}
                                    onClick={() => this.props.onClick(i, j)}
                                    key={i + j}
                                />
                            )
                        )}
                    </div>
                </div>
            </div>
        );
    }
}

export default Board;