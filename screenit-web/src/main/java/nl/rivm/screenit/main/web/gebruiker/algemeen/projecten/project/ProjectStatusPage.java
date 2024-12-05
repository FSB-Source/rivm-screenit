package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.project;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectPaspoortPanel;
import nl.rivm.screenit.model.ProjectParameter;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.rivm.screenit.util.OrganisatieUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.BooleanLabel;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ProjectStatusPage extends ProjectBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public ProjectStatusPage(IModel<Project> model)
	{
		super(new CompoundPropertyModel<>(model));
		Project project = model.getObject();
		List<Bevolkingsonderzoek> bevolkingsonderzoeken = model.getObject().getBevolkingsonderzoeken();

		add(ComponentHelper.newLabel("id"));
		add(ComponentHelper.newLabel("projectNaam", new PropertyModel<Project>(model, "naam")));
		add(ComponentHelper.newLabel("naam"));
		add(ComponentHelper.newLabel("organisatie.naam"));

		add(new Label("bevolkingsonderzoeken", Model.of(Bevolkingsonderzoek.getAfkortingen(bevolkingsonderzoeken))));

		add(ComponentHelper.newLabel("contactpersoon.medewerker.naamVolledig"));
		add(new Label("medewerkers", Model.of(MedewerkerUtil.getMedewerkerNamen(model.getObject().getMedewerkers()))));
		add(new Label("screeningorganisaties", Model.of(OrganisatieUtil.getOrganisatieNamen(model.getObject().getScreeningOrganisaties()))));

		add(DateLabel.forDatePattern("startDatum", "dd-MM-yyyy"));
		add(DateLabel.forDatePattern("eindDatum", "dd-MM-yyyy"));
		add(DateLabel.forDatePattern("eindeInstroom", "dd-MM-yyyy"));
		add(new BooleanLabel("anoniem"));

		toonProjectParameters(project);

		add(new Label("opmerkingen"));

		add(new AjaxLink<Void>("projectbewerken")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ProjectEditPage(getProjectModel()));
			}

			@Override
			public void onConfigure()
			{
				super.onConfigure();
				Project project = getProjectModel().getObject();
				Date eindDatum = project.getEindDatum();
				Date nu = currentDateSupplier.getDate();
				boolean level = getToegangsLevel(Recht.GEBRUIKER_PROJECT_OVERZICHT, Actie.AANPASSEN) != null;
				setVisible(!eindDatum.before(nu) && level);
			}

		});

		add(new ProjectPaspoortPanel("projectPasspoort", model));
	}

	private void toonProjectParameters(Project project)
	{
		boolean parameterLijstIsLeeg = project.getParameters().isEmpty();

		WebMarkupContainer parametersTitel = new WebMarkupContainer("parametersTitel");
		parametersTitel.setVisible(project.getType().equals(ProjectType.PROJECT));
		add(parametersTitel);
		List<ProjectParameter> parameters = new ArrayList<>(project.getParameters());
		parameters.sort(Comparator.comparing(ProjectParameter::getKey));

		ListView<ProjectParameter> parameterList = new ListView<ProjectParameter>("parameters", ModelUtil.listRModel(parameters))
		{
			@Override
			protected void populateItem(ListItem<ProjectParameter> item)
			{
				ProjectParameter parameter = item.getModelObject();
				item.setDefaultModel(new CompoundPropertyModel<>(item.getModel()));
				item.add(new EnumLabel<ProjectParameterKey>("key"));
				Label valueLabel = new Label("value");
				EnumLabel<ColonOnderzoeksVariant> onderzoeksvariantLabel = maakOnderzoeksvariantLabel(item.getModel());

				if (!parameter.getKey().getValueType().equals(ColonOnderzoeksVariant.class))
				{
					onderzoeksvariantLabel.setVisible(false);
				}
				else
				{
					valueLabel.setVisible(false);
				}
				item.add(valueLabel);
				item.add(onderzoeksvariantLabel);

				Label eenheidLabel = new Label("unit", getString(EnumStringUtil.getPropertyString(parameter.getKey()) + ".unit"));
				eenheidLabel.setVisible(parameter.getValue() != null);
				item.add(eenheidLabel);
			}
		};
		parameterList.setVisible(!parameterLijstIsLeeg && project.getType().equals(ProjectType.PROJECT));
		add(parameterList);

		WebMarkupContainer geenParametersLabel = new WebMarkupContainer("geenParameters");
		geenParametersLabel.setVisible(parameterLijstIsLeeg && project.getType().equals(ProjectType.PROJECT));
		add(geenParametersLabel);
	}

	private EnumLabel<ColonOnderzoeksVariant> maakOnderzoeksvariantLabel(IModel<ProjectParameter> parameterModel)
	{

		EnumLabel<ColonOnderzoeksVariant> onderzoeksvariantLabel = new EnumLabel<>("enumValue");

		onderzoeksvariantLabel.setModel(new IModel<ColonOnderzoeksVariant>()
		{
			@Override
			public ColonOnderzoeksVariant getObject()
			{
				ProjectParameter parameter = parameterModel.getObject();
				String value = parameter.getValue();
				return StringUtils.isNotBlank(value) ? ColonOnderzoeksVariant.valueOf(value) : null;
			}

			@Override
			public void setObject(ColonOnderzoeksVariant object)
			{
				ProjectParameter parameter = parameterModel.getObject();
				parameter.setValue(object.name());
			}
		});
		return onderzoeksvariantLabel;
	}
}
