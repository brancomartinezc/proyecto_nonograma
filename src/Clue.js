import React from 'react';

class Clue extends React.Component {
    constructor(props) {
        super(props);

        this.state = {
          satisfies: false
        };
    }

    render() {
        if (this.props.sat && !this.state.satisfies) {
            // Si esta pista fue satisfecha en una jugada y era falsa antes, pintar de verde
            this.setState({satisfies: this.props.sat});
        } else if (!this.props.sat && this.props.satisfies) {
            // Si esta pista ya estaba pintada y por una jugada dejo de satisfacerse, despintarla
            this.setState({satisfies: this.props.sat});
        }

        const clue = this.props.clue;
        const sat = this.state.satisfies;
        return (
            <div className={`clue ${sat ? "satisfies" : ""}`} >
                {clue.map((num, i) =>
                    <div key={i}>
                        {num}
                    </div>
                )}
            </div>
        );
    }
}

export default Clue;