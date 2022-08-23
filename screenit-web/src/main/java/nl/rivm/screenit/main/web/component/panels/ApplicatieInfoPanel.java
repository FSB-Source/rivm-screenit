package nl.rivm.screenit.main.web.component.panels;

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

import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import nl.rivm.screenit.ApplicationEnvironment;
import nl.rivm.screenit.main.web.ScreenitApplication;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ApplicatieInfoPanel extends Panel
{
	private static final long serialVersionUID = 1L;

	@SpringBean(name = "applicationEnvironment")
	private String applicationEnvironment;

	@SpringBean(name = "applicationName")
	private String applicationName;

	@SpringBean(name = "applicationInstance")
	private String applicationInstance;

	public ApplicatieInfoPanel(String id)
	{
		super(id);

		add(new Label("version", ScreenitApplication.get().getVersionString()));
		add(new Label("name", applicationName));

		WebMarkupContainer environmentContainer = new WebMarkupContainer("environmentContainer");
		environmentContainer.setOutputMarkupPlaceholderTag(Boolean.TRUE);

		String omgevingTekst = omgevingTekst();
		environmentContainer.add(new Label("environment", omgevingTekst).setVisible(StringUtils.isNotBlank(omgevingTekst)));
		add(environmentContainer);

	}

	private String omgevingTekst()
	{
		String omgevingTekst = ApplicationEnvironment.PROD.getEnvNaam().equalsIgnoreCase(applicationEnvironment) ? null : applicationEnvironment;
		return Stream.of(omgevingTekst, applicationInstance).filter(Objects::nonNull).collect(Collectors.joining(" - "));
	}
}
