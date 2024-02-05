package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole.reeks;

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

import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.SKMLSentineelControleBarcode;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.KwaliteitscontroleService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_BEHEER_SENTINELCONTROLES,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class SentinelControlesPage extends KwaliteitscontroleBasePage
{

	private static final long serialVersionUID = 1L;

	private IModel<List<SKMLSentineelControleBarcode>> allSentineelsModels;

	@SpringBean
	private KwaliteitscontroleService kwaliteitscontroleService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public SentinelControlesPage()
	{
		add(new SentineelcontrolesForm("form"));
	}

	private class SentineelcontrolesForm extends ScreenitForm<SKMLSentineelControleBarcode>
	{

		private static final long serialVersionUID = 1L;

		public SentineelcontrolesForm(String id)
		{
			super(id);

			List<SKMLSentineelControleBarcode> allSentineels = kwaliteitscontroleService.createOrGetAllSentineelControles();
			allSentineelsModels = ModelUtil.listModel(allSentineels);

			List<SKMLSentineelControleBarcode> controleTypesSet1 = filterSentineelControleBarcodes(allSentineels, 1);
			List<SKMLSentineelControleBarcode> controleTypesSet2 = filterSentineelControleBarcodes(allSentineels, 2);

			IModel<List<SKMLSentineelControleBarcode>> sentinelsSet1 = ModelUtil.listModel(controleTypesSet1);
			add(new SentinelInvoerPanel("set1", sentinelsSet1, "Set 1"));

			IModel<List<SKMLSentineelControleBarcode>> sentinelsSet2 = ModelUtil.listModel(controleTypesSet2);
			add(new SentinelInvoerPanel("set2", sentinelsSet2, "Set 2"));

			add(new IndicatingAjaxButton("opslaan")
			{
				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					for (SKMLSentineelControleBarcode cup : allSentineelsModels.getObject())
					{
						cup.setDatum(currentDateSupplier.getDate());
						hibernateService.saveOrUpdate(cup);
					}
					logService.logGebeurtenis(LogGebeurtenis.SENTINEL_BARCODES_GEWIJZIGD, ScreenitSession.get().getLoggedInAccount(), Bevolkingsonderzoek.COLON);
					info("ID's zijn opgeslagen.");
				}
			});
		}

		private List<SKMLSentineelControleBarcode> filterSentineelControleBarcodes(List<SKMLSentineelControleBarcode> allSentineels, int setNr)
		{
			return allSentineels.stream().filter(controleBarcode -> controleBarcode.getSentineelType().getSetNr() == setNr).collect(Collectors.toList());
		}

		@Override
		protected void onDetach()
		{
			super.onDetach();
			ModelUtil.nullSafeDetach(allSentineelsModels);
		}

	}
}
