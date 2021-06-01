import React from 'react';

class Mode extends React.Component {
    render() {
        return (
            <button className={this.props.class} onClick={this.props.onClick} disabled={this.props.gameWon}>
                {this.props.value}
            </button>
        );
    }
}

export default Mode;