package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.review;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBeoordelenPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.werklijst.MiniWerklijstPanel;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaConclusieReview;

import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaConclusieReviewMiniWerklijstPanel extends MiniWerklijstPanel
{
	@SpringBean
	private MammaConclusieReviewService conclusieReviewService;

	public MammaConclusieReviewMiniWerklijstPanel(String id, AbstractMammaBeoordelenPage parent, Long huidigeBeoordelingId, List<Long> beoordelingenIds)
	{
		super(id, parent, huidigeBeoordelingId, beoordelingenIds);
	}

	@Override
	protected PropertyColumn<MammaBeoordeling, String> maakStatusKolom()
	{
		return new PropertyColumn<>(Model.of("Status"), "status")
		{
			@Override
			public IModel<String> getDataModel(IModel<MammaBeoordeling> beoordelingModel)
			{
				MammaConclusieReview conclusieReview = conclusieReviewService.getConclusieReview(beoordelingModel.getObject().getOnderzoek().getAfspraak().getUitnodiging()
					.getScreeningRonde(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				return conclusieReview != null ? Model.of(getString("conclusie.gezien.status")) : Model.of(getString("conclusie.niet.gezien.status"));
			}
		};
	}
}
