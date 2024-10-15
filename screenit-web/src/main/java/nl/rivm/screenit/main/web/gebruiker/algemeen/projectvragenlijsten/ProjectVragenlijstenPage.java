package nl.rivm.screenit.main.web.gebruiker.algemeen.projectvragenlijsten;

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
import java.util.List;

import nl.rivm.screenit.main.service.VragenlijstService;
import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.project.ProjectVragenlijst;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_DEFINITIE_VRAGENLIJSTEN },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class ProjectVragenlijstenPage extends ProjectVragenlijstenBasePage
{
	@SpringBean
	private VragenlijstService vragenlijstService;

	@SpringBean
	private ProjectService projectService;

	public ProjectVragenlijstenPage()
	{
		final WebMarkupContainer refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		ProjectVragenlijst zoekObject = new ProjectVragenlijst();
		zoekObject.setActief(true);
		IModel<ProjectVragenlijst> modelZoekObject = Model.of(zoekObject);
		List<IColumn<ProjectVragenlijst, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Naam vragenlijst"), "naam", "naam"));
		columns.add(new PropertyColumn<>(Model.of("Laatst gewijzigd"), "laatstGewijzigd", "laatstGewijzigd"));

		BootstrapDialog confirmDialog = new BootstrapDialog("confirmDialog");
		add(confirmDialog);

		columns.add(new ActiefPropertyColumn<>(Model.of(""), "actief", refreshContainer, modelZoekObject,
			ScreenitSession.get().checkPermission(Recht.GEBRUIKER_DEFINITIE_VRAGENLIJSTEN, Actie.VERWIJDEREN), confirmDialog, "label.vragenlijstdeactiveren")
		{

			@Override
			protected void onAfterToggleActief(AjaxRequestTarget target, ProjectVragenlijst actiefObject)
			{
				super.onAfterToggleActief(target, actiefObject);
				try
				{
					vragenlijstService.updateVragenlijst(actiefObject, null, null);
				}
				catch (Exception e)
				{

				}
			}

			@Override
			protected boolean mayToggle(ProjectVragenlijst actiefObject)
			{
				boolean mayToggle = Boolean.FALSE.equals(actiefObject.getActief()) || !projectService.isVragenlijstGekoppeldAanProject(actiefObject.getId())
					|| !projectService.isVragenlijstGekoppeldAanNietBeeindigdProject(actiefObject.getId());

				if (!mayToggle)
				{
					ScreenitSession.get().warn("Vragenlijst kan niet worden gedeactiveerd, omdat deze al/nog in gebruik is.");
				}

				return mayToggle;
			}

		});

		ScreenitDataTable<ProjectVragenlijst, String> organisaties = new ScreenitDataTable<ProjectVragenlijst, String>("vragenlijsten", columns,
			new VragenlijstenProvider<>(modelZoekObject, "naam"), 10, Model.of("vragenlijst(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<ProjectVragenlijst> model)
			{
				setResponsePage(new ProjectVragenlijstEditPage(ModelUtil.cModel(model.getObject())));
			}

		};
		refreshContainer.add(organisaties);

		add(new IndicatingAjaxLink<ProjectVragenlijst>("newVragenlijst")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ProjectVragenlijstEditPage(ModelUtil.cModel(new ProjectVragenlijst())));
			}

		}.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_DEFINITIE_VRAGENLIJSTEN, Actie.TOEVOEGEN)));
	}
}
