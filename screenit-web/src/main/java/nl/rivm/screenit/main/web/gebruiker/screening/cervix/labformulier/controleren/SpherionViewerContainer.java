package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

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

import java.util.Date;

import nl.topicuszorg.wicket.component.object.PdfObjectContainer;

import org.apache.wicket.IRequestListener;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.request.resource.IResource;
import org.apache.wicket.request.resource.IResource.Attributes;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.time.Duration;

public class SpherionViewerContainer extends PdfObjectContainer implements IRequestListener
{

	private static final long serialVersionUID = 1L;

	private final String objid;

	@SpringBean(name = "spherionUrl")
	private String spherionUrl;

	@SpringBean(name = "spherionUsername")
	private String spherionUsername;

	@SpringBean(name = "spherionPassword")
	private String spherionPassword;

	public SpherionViewerContainer(String id, String objid)
	{
		super(id);
		this.objid = objid;

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
		Duration cacheDuration = Duration.minutes(30); 
		IResource resource = new SpherionFormulierViewerResource(String.format(spherionUrl, objid), false, cacheDuration, spherionUsername, spherionPassword);
		Attributes a = new Attributes(RequestCycle.get().getRequest(), RequestCycle.get().getResponse(), null);
		resource.respond(a);
	}

	@Override
	public boolean rendersPage()
	{
		return false;
	}

}
