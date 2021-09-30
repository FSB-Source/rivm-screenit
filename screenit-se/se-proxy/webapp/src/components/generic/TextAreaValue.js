/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import React from 'react';
import {Input} from 'reactstrap';

type TextValueProps = {
    value: string;
    maxLength?: number,
    className?: string,
    placeholder?: string,
    disabled: boolean,
    color?: string,

    onChange: (value: string) => mixed;
}

type TextValueState = {
    value: string;
}

export default class TextAreaValue extends React.Component<TextValueProps, TextValueState> {

    constructor(props: TextValueProps) {
        super(props);
        this.props = props;
        this.state = {
            value: props.value,
        };
    }

    componentDidUpdate(prevProps: TextValueProps) {
        if(this.props.value !== prevProps.value && this.props.value !== this.state.value) {
            this.setState({
                value: this.props.value,
            });
        }
    }

    updateValue(event: Event) {
        let target = event.target;
        if (target instanceof HTMLTextAreaElement) {
            this.setState({
                value: target.value,
            });
            this.props.onChange(target.value);
        }
    }

    render() {
        return (
            <div className={this.props.className}>
                <Input type={'textarea'}
                       disabled={this.props.disabled}
                       value={this.state.value}
                       placeholder={this.props.placeholder}
                       onChange={this.updateValue.bind(this)}
                       maxLength={this.props.maxLength}
                       style={{backgroundColor: this.props.color}}
                />
            </div>
        );
    }
}
