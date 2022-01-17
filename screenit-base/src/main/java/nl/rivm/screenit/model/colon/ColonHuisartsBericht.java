package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.HuisartsBericht;
import nl.rivm.screenit.model.OnbekendeHuisarts;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "colon")
@Audited
public class ColonHuisartsBericht extends HuisartsBericht
{
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST, optional = true)
	@NotAudited
	private EnovationHuisarts huisarts;

	@Deprecated
	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST, optional = true)
	@NotAudited
	private OnbekendeHuisarts onbekendeHuisarts;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST, optional = false)
	private ColonScreeningRonde screeningsRonde;

	@Enumerated(EnumType.STRING)
	private ColonHuisartsBerichtStatus status;

	private Date verzendDatum;

	@Transient
	private boolean opnieuwVerzonden;

	public EnovationHuisarts getHuisarts()
	{
		return huisarts;
	}

	public void setHuisarts(EnovationHuisarts huisarts)
	{
		this.huisarts = huisarts;
	}

	@Deprecated
	public OnbekendeHuisarts getOnbekendeHuisarts()
	{
		return onbekendeHuisarts;
	}

	@Deprecated
	public void setOnbekendeHuisarts(OnbekendeHuisarts onbekendeHuisarts)
	{
		this.onbekendeHuisarts = onbekendeHuisarts;
	}

	public ColonScreeningRonde getScreeningsRonde()
	{
		return screeningsRonde;
	}

	public void setScreeningsRonde(ColonScreeningRonde screeningsRonde)
	{
		this.screeningsRonde = screeningsRonde;
	}

	public ColonHuisartsBerichtStatus getStatus()
	{
		return status;
	}

	public void setStatus(ColonHuisartsBerichtStatus status)
	{
		this.status = status;
	}

	public Date getVerzendDatum()
	{
		return verzendDatum;
	}

	public void setVerzendDatum(Date verzendDatum)
	{
		this.verzendDatum = verzendDatum;
	}

	public boolean isOpnieuwVerzonden()
	{
		return opnieuwVerzonden;
	}

	public void setOpnieuwVerzonden(boolean opnieuwVerzonden)
	{
		this.opnieuwVerzonden = opnieuwVerzonden;
	}
}
