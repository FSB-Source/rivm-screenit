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
import Select from 'react-select';

type DropdownValueProps<T> = {
    id: string,
    value: ?T,
    disabled: boolean,
    options: Array<T>,
    required?: boolean,
    isWhite?: boolean,
    placeholder?: string,
    lavendel?: boolean,
    valueToLabel?: (value: T) => string;

    handleChange: (value: ?T) => void;
}

export default class DropdownValue<T> extends React.Component<DropdownValueProps<T>> {

    constructor(props: DropdownValueProps<T>) {
        super(props);
        this.props = props;

        this.handleChange = this.handleChange.bind(this);
    }

    handleChange = (selectedOption: ?{ value: T, label: string }): void => {
        this.props.handleChange(selectedOption ? selectedOption.value : null);
    };

    render() {
        return (
            <div className={this.props.lavendel ? 'select-container-lavendel' : ''}
                 title={this.props.value ? this.props.valueToLabel ? this.props.valueToLabel(this.props.value) : this.props.value : ''}>
                <Select id={this.props.id} style={{backgroundColor: (this.props.isWhite ? '#fff' : 'lavender'), color: '#000!important'}}
                        placeholder={this.props.placeholder ? this.props.placeholder : 'Kies...'}
                        value={this.props.value ?
                            {
                                value: this.props.value,
                                label: this.props.valueToLabel ? this.props.valueToLabel(this.props.value) : this.props.value,
                            } :
                            null}
                        onChange={this.handleChange}
                        options={this.props.options.map((v) => {
                            return {value: v, label: this.props.valueToLabel ? this.props.valueToLabel(v) : v};
                        })}
                        isDisabled={this.props.disabled}
                        isClearable={!this.props.required}
                        noOptionsMessage={() => 'Geen resultaten'}>
                </Select>
            </div>
        );
    }
}
