
package nl.rivm.screenit.model.colon;

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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.enums.BriefType;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon")
@Audited
@Getter
@Setter
public class ColonAfmelding extends Afmelding<ColonScreeningRonde, ColonDossier, ColonBrief>
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = true)
	@Cascade(CascadeType.SAVE_UPDATE)
	private ColonScreeningRonde screeningRonde;

	@ManyToOne(optional = true)
	@Cascade(CascadeType.SAVE_UPDATE)
	private ColonDossier dossier;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade(CascadeType.ALL)
	private ColonBrief afmeldingAanvraag;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade(CascadeType.ALL)
	private ColonBrief afmeldingBevestiging;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade(CascadeType.ALL)
	private ColonBrief heraanmeldAanvraag;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@Cascade(CascadeType.ALL)
	private ColonBrief heraanmeldBevestiging;

	@OneToMany(mappedBy = "afmelding", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@Cascade(CascadeType.ALL)
	private List<ColonBrief> brieven = new ArrayList<>();

	@Enumerated(EnumType.STRING)
	private ColonAfmeldingReden reden;

	@Transient
	private ColonIntakeAfspraak heraanmeldingAfspraak;

	@Transient
	private Boolean heraanmeldingAfspraakUitRooster;

	@Transient
	private BriefType heraanmeldingAfspraakBriefType;

	@Transient
	private Boolean heraanmeldingAfspraakBriefTegenhouden;

	@Transient
	private Boolean heraanmeldingBevestigingsBriefTegenhouden;

	@Transient
	private int tijdelijkAfmeldenTotJaartal;
}
