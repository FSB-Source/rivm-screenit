package nl.rivm.screenit.wsb.fhir.servlet.dstu3.v1;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.inject.Singleton;

import nl.rivm.screenit.wsb.fhir.interceptor.FhirCertificaatInterceptor;
import nl.rivm.screenit.wsb.fhir.interceptor.ProcessingCompletedInterceptor;
import nl.rivm.screenit.wsb.fhir.provider.dstu3.v1.BundleProvider;
import nl.rivm.screenit.wsb.fhir.provider.dstu3.v1.TaskProvider;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.rest.server.IResourceProvider;
import ca.uhn.fhir.rest.server.RestfulServer;
import ca.uhn.fhir.rest.server.interceptor.ResponseHighlighterInterceptor;

@Singleton
public class RestfulServlet extends RestfulServer
{

	private static final long serialVersionUID = -3931111342737918913L;

	public RestfulServlet()
	{
		super(FhirContext.forDstu3());
	}

	@Override
	public void initialize()
	{
		setResourceProviders(getProviders());

		registerInterceptor(new ResponseHighlighterInterceptor());
		registerInterceptor(new FhirCertificaatInterceptor());
		registerInterceptor(new ProcessingCompletedInterceptor());

	}

	private List<IResourceProvider> getProviders()
	{
		List<IResourceProvider> providers = new ArrayList<>();

		providers.add(new BundleProvider());
		providers.add(new TaskProvider());

		return providers;
	}

}
