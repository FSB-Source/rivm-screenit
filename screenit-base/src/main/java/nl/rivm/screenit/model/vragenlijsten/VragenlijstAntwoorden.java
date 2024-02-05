
package nl.rivm.screenit.model.vragenlijsten;

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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "algemeen")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class VragenlijstAntwoorden<VH extends VragenlijstAntwoordenHolder> extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL, optional = false)
	private ScreenitFormulierInstantie formulierInstantie;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	private FormulierResultaatImpl resultaat;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "vragenlijstAntwoorden", targetEntity = VragenlijstAntwoordenHolder.class)
	private VH antwoordenHolder;

	public ScreenitFormulierInstantie getFormulierInstantie()
	{
		return formulierInstantie;
	}

	public void setFormulierInstantie(ScreenitFormulierInstantie formulierInstantie)
	{
		this.formulierInstantie = formulierInstantie;
	}

	public FormulierResultaatImpl getResultaat()
	{
		return resultaat;
	}

	public void setResultaat(FormulierResultaatImpl resultaat)
	{
		this.resultaat = resultaat;
	}

	public VH getAntwoordenHolder()
	{
		return antwoordenHolder;
	}

	public void setAntwoordenHolder(VH antwoordenHolder)
	{
		this.antwoordenHolder = antwoordenHolder;
	}
}
