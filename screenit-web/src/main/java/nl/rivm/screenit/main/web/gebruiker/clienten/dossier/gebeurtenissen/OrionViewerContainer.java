package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.time.Duration;
import java.util.Date;

import nl.topicuszorg.wicket.component.object.PdfObjectContainer;

import org.apache.wicket.IRequestListener;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.resource.IResource;
import org.apache.wicket.request.resource.IResource.Attributes;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class OrionViewerContainer extends PdfObjectContainer implements IRequestListener
{

	private static final long serialVersionUID = 1L;

	private String objid;

	private IModel<File> formulier;

	@SpringBean(name = "antwoordFormulierUrl")
	private String antwoordFormulierUrl;

	public OrionViewerContainer(String id, String objid)
	{
		super(id);
		this.objid = objid;

		Injector.get().inject(this);
	}

	public OrionViewerContainer(String id, Model<File> formulier)
	{
		super(id);
		this.formulier = formulier;
		Injector.get().inject(this);
	}

	@Override
	protected void onInitialize()
	{

		setValue(DATA_ATTRIBUTE, urlForListener(null).toString() + "&random=" + new Date().getTime());
		super.onInitialize();
	}

	@Override
	public final void onRequest()
	{
		Duration cacheDuration = Duration.ofMinutes(30); 

		IResource resource;
		if (formulier != null)
		{
			resource = new ScannedFormulierViewerResource(formulier, false, cacheDuration);
		}
		else
		{
			resource = new ScannedFormulierViewerResourceExternal(String.format(antwoordFormulierUrl, objid), false, cacheDuration);
		}

		Attributes a = new Attributes(RequestCycle.get().getRequest(), RequestCycle.get().getResponse(), null);
		resource.respond(a);
	}

	@Override
	public boolean rendersPage()
	{
		return false;
	}

}
