package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.colon.enums.RetourzendingStatus;
import nl.rivm.screenit.model.colon.enums.RetourzendingWijze;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
@Getter
@Setter
public abstract class InpakbareUitnodiging<SR extends ScreeningRonde<?, ?, ?, ?>> extends Uitnodiging<SR>
{
	@Column(nullable = false)
	private Long uitnodigingsId;

	private boolean verstuurd;

	private boolean verstuurdDoorInpakcentrum;

	@Deprecated
	@Temporal(TemporalType.TIMESTAMP)
	private Date datumTerugOntvangen;

	@Temporal(TemporalType.TIMESTAMP)
	private Date retourOntvangen;

	private String retourzendingReden;

	@Enumerated(EnumType.STRING)
	private RetourzendingStatus retourzendingStatus;

	@Enumerated(EnumType.STRING)
	private RetourzendingWijze retourzendingWijze;

	private String templateNaam;

	private String trackTraceId;

	@Temporal(TemporalType.TIMESTAMP)
	private Date verstuurdDatum;
}
