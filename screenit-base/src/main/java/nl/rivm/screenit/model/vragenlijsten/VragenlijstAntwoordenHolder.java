
package nl.rivm.screenit.model.vragenlijsten;

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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.TablePerClassHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity()
@Table(schema = "gedeeld")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public abstract class VragenlijstAntwoordenHolder<V extends Vragenlijst, VH extends VragenlijstAntwoordenHolder> extends TablePerClassHibernateObject
{

	private static final long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY, targetEntity = VragenlijstAntwoorden.class)
	private VragenlijstAntwoorden<VH> vragenlijstAntwoorden;

	public VragenlijstAntwoorden<VH> getVragenlijstAntwoorden()
	{
		return vragenlijstAntwoorden;
	}

	public void setVragenlijstAntwoorden(VragenlijstAntwoorden<VH> vragenlijstAntwoorden)
	{
		this.vragenlijstAntwoorden = vragenlijstAntwoorden;
	}

	public abstract V getVragenlijst();

	public abstract void setVragenlijst(V vragenlijst);

}
