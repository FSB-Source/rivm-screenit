
package nl.rivm.screenit.main.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class IFobtBatchFilter implements IDetachable
{

	private static final long serialVersionUID = 1L;

	private IFOBTBestandStatus status;

	private Date datumVan;

	private Date datumTot;

	private IModel<IFobtLaboratorium> lab;

	private boolean analyseDatum;

	public IFOBTBestandStatus getStatus()
	{
		return status;
	}

	public void setStatus(IFOBTBestandStatus status)
	{
		this.status = status;
	}

	public Date getDatumVan()
	{
		return datumVan;
	}

	public void setDatumVan(Date datumVan)
	{
		this.datumVan = datumVan;
	}

	public Date getDatumTot()
	{
		return datumTot;
	}

	public void setDatumTot(Date datumTot)
	{
		this.datumTot = datumTot;
	}

	public IFobtLaboratorium getLab()
	{
		return ModelUtil.nullSafeGet(lab);
	}

	public void setLab(IFobtLaboratorium lab)
	{
		this.lab = ModelUtil.sModel(lab);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(lab);
	}

	public boolean isAnalyseDatum()
	{
		return analyseDatum;
	}

	public void setAnalyseDatum(boolean analyseDatum)
	{
		this.analyseDatum = analyseDatum;
	}

}
