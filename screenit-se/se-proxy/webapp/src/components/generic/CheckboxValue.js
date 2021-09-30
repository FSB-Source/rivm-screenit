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
import {Form, FormGroup, Input, Label} from 'reactstrap';

type CheckboxValueProps = {
    label: string;
    checked: boolean;
    disabled: boolean;
    handleChange: (value: boolean) => void;
}

export default class CheckboxValue extends React.Component<CheckboxValueProps> {

    constructor(props: CheckboxValueProps) {
        super(props);
        this.props = props;
        this.handleChange.bind(this);
    }

    handleChange = (event: Event): void => {
        if (event.target instanceof HTMLInputElement) {
            this.props.handleChange(event.target.checked);
        }
    };

    render() {
        return (
            <Form>
                <FormGroup check inline>
                    <Label check>
                        <Input type='checkbox' className='se-value' checked={this.props.checked} onChange={this.handleChange} disabled={this.props.disabled}/>
                        {this.props.label}
                    </Label>
                </FormGroup>
            </Form>
        );
    }
}
