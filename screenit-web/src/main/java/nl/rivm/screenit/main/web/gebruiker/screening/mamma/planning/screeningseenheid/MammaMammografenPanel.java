package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.NotClickablePropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.mamma.MammaMammograaf;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.mamma.RegioUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaMammografenPanel extends GenericPanel<MammaScreeningsEenheid>
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	private final BootstrapDialog dialog;

	private ScreenitDataTable<MammaMammograaf, String> mammografentabel;

	private final MammaMammografenProvider mammografenProvider;

	MammaMammografenPanel(final String id, IModel<MammaScreeningsEenheid> screeningsEenheidModel, final boolean magAanpassen)
	{
		super(id, screeningsEenheidModel);
		mammografenProvider = new MammaMammografenProvider(new PropertyModel<>(getModel(), "mammografen"));

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		AjaxLink<Void> createMammograafBtn = new IndicatingAjaxLink<Void>("createMammograaf")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				final MammaScreeningsEenheid se = MammaMammografenPanel.this.getModelObject();
				final String drieLetterRegioAfkorting = RegioUtil.drieLetterRegioafkorting(ScreenitSession.get().getScreeningOrganisatie().getRegioCode());
				final String aeTitle = drieLetterRegioAfkorting + "-" + se.getCode() + "-MG" + String.format("%02d", mammografenProvider.size() + 1);
				openMammograafDialog(target, ModelUtil.cModel(new MammaMammograaf().setScreeningsEenheid(se).setAeTitle(aeTitle)), mammografenProvider, true);
			}
		};
		createMammograafBtn.setVisible(magAanpassen);
		createMammograafBtn.setOutputMarkupId(true);
		add(createMammograafBtn);

		List<IColumn<MammaMammograaf, String>> columns = new ArrayList<>();
		if (magAanpassen)
		{
			columns.add(new PropertyColumn<>(Model.of("AE-title"), "aeTitle"));
			columns.add(new PropertyColumn<>(Model.of("IP-adres van het bijbehorende werkstation"), "werkstationIpAdres"));
			columns.add(new NotClickablePropertyColumn<MammaMammograaf, String>(Model.of(""), "")
			{
				@Override
				public void populateItem(Item<ICellPopulator<MammaMammograaf>> cell, String componentId, IModel<MammaMammograaf> mammograafModel)
				{
					cell.add(new DeleteMammograafPanel(componentId, mammograafModel)
					{
						@Override
						protected void delete(final AjaxRequestTarget target, final IModel<MammaMammograaf> model)
						{
							screeningsEenheidService.deleteMammograaf(model.getObject(), MammaMammografenPanel.this.getModelObject());
							target.add(mammografentabel);
						}
					});
				}
			});
		}
		else
		{
			columns.add(new NotClickablePropertyColumn<>(Model.of("AE-title"), "aeTitle"));
			columns.add(new NotClickablePropertyColumn<>(Model.of("IP-adres van het bijbehorende werkstation"), "werkstationIpAdres"));
		}
		mammografentabel = new ScreenitDataTable<MammaMammograaf, String>("mammografentabel", columns, mammografenProvider, 999, null, false)
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaMammograaf> model)
			{
				openMammograafDialog(target, ModelUtil.cModel(model.getObject()), mammografenProvider, false);
			}
		};
		mammografentabel.setOutputMarkupId(true);
		add(mammografentabel);
	}

	private void openMammograafDialog(AjaxRequestTarget target, IModel<MammaMammograaf> model, MammaMammografenProvider mammografenProvider, boolean create)
	{
		dialog.openWith(target, new MammaMammograafEditPanel(BootstrapDialog.CONTENT_ID, model)
		{
			@Override
			void opslaan(final AjaxRequestTarget target)
			{
				dialog.close(target);
				target.add(mammografentabel);
			}

			@Override
			void annuleren(AjaxRequestTarget target)
			{
				dialog.close(target);
			}
		});
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		mammografenProvider.detach();
	}
}
