package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;

import org.hibernate.envers.Audited;

@Entity
@Audited
@Getter
@Setter
public abstract class DigitaalClientBericht<SR extends ScreeningRonde> extends TablePerClassHibernateObject
{
	@Column(nullable = false)
	private LocalDateTime creatieMoment;

	@Column(nullable = false)
	private Boolean isHerzonden = false;

	@Column(nullable = false, length = GbaPersoon.MAX_EMAIL_LENGTH)
	private String ontvanger;

	@Column(nullable = false)
	private Boolean verzendenGefaald;

	@Column(length = 100)
	private String omschrijving;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private DigitaalBerichtTemplateType digitaalBerichtTemplateType;

	public abstract SR getScreeningRonde();

	public abstract void setScreeningRonde(SR screeningRonde);
}
