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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

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

	@OneToOne(optional = true, mappedBy = "dossier", fetch = FetchType.LAZY)
	@Cascade(org.hibernate.annotations.CascadeType.DELETE)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private ColonVolgendeUitnodiging volgendeUitnodiging;

	public ColonVooraankondiging getColonVooraankondiging()
	{
		return colonVooraankondiging;
	}

	public void setColonVooraankondiging(ColonVooraankondiging colonVooraankondiging)
	{
		this.colonVooraankondiging = colonVooraankondiging;
	}

	public InactiveerReden getInactiveerReden()
	{
		return inactiveerReden;
	}

	public void setInactiveerReden(InactiveerReden inactiveerReden)
	{
		this.inactiveerReden = inactiveerReden;
	}

	@Override
	public Client getClient()
	{
		return client;
	}

	@Override
	public void setClient(Client client)
	{
		this.client = client;
	}

	@Override
	public List<ColonScreeningRonde> getScreeningRondes()
	{
		return screeningRondes;
	}

	@Override
	public void setScreeningRondes(List<ColonScreeningRonde> screeningRondes)
	{
		this.screeningRondes = screeningRondes;
	}

	@Override
	public ColonScreeningRonde getLaatsteScreeningRonde()
	{
		return laatsteScreeningRonde;
	}

	@Override
	public void setLaatsteScreeningRonde(ColonScreeningRonde laatsteScreeningRonde)
	{
		this.laatsteScreeningRonde = laatsteScreeningRonde;
	}

	@Override
	public List<ColonAfmelding> getAfmeldingen()
	{
		return afmeldingen;
	}

	@Override
	public void setAfmeldingen(List<ColonAfmelding> afmeldingen)
	{
		this.afmeldingen = afmeldingen;
	}

	@Override
	public ColonAfmelding getLaatsteAfmelding()
	{
		return laatsteAfmelding;
	}

	@Override
	public void setLaatsteAfmelding(ColonAfmelding laatsteAfmelding)
	{
		this.laatsteAfmelding = laatsteAfmelding;
	}

	public ColonVolgendeUitnodiging getVolgendeUitnodiging()
	{
		return volgendeUitnodiging;
	}

	public void setVolgendeUitnodiging(ColonVolgendeUitnodiging volgendeUitnodiging)
	{
		this.volgendeUitnodiging = volgendeUitnodiging;
	}
}
