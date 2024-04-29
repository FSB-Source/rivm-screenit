package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.colon.enums.InactiveerReden;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "colon")
@Audited
@Getter
@Setter
public class ColonDossier extends Dossier<ColonScreeningRonde, ColonAfmelding>
{
	@OneToOne(mappedBy = "colonDossier", optional = false)
	private Client client;

	@Enumerated(EnumType.STRING)
	private InactiveerReden inactiveerReden;

	@OneToOne(optional = true, cascade = CascadeType.ALL)
	@NotAudited
	private ColonVooraankondiging colonVooraankondiging;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "dossier", cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ColonScreeningRonde> screeningRondes = new ArrayList<>();

	@OneToOne(optional = true, cascade = CascadeType.ALL)
	private ColonScreeningRonde laatsteScreeningRonde;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "dossier")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ColonAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(optional = true, cascade = CascadeType.ALL)
	private ColonAfmelding laatsteAfmelding;

	@OneToOne(optional = true, mappedBy = "dossier", fetch = FetchType.LAZY, cascade = javax.persistence.CascadeType.REMOVE)
	@Cascade(org.hibernate.annotations.CascadeType.DELETE)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private ColonVolgendeUitnodiging volgendeUitnodiging;
}
